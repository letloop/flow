(library (letloop flow entangle)

  (export make-entangle
          entangle-abort
          entangle-run
          entangle-sleep
          entangle-spawn
          entangle-spawn-threadsafe
          entangle-stop
          #;entangle-tcp-connect
          entangle-tcp-serve
          )

  (import (chezscheme)
          (letloop r999)
          (letloop epoll)
          (letloop cffi))
  ;;
  ;; inspired from https://stackoverflow.com/a/51777980/140837
  ;;
  ;; single thread, single event-loop
  ;;

  (define libloop
    (let ()
      ;; load libc before libloop
      (load-shared-object #f)
      ;; TODO: avoid the need to load liburing, when only using epoll
      (load-shared-object "liburing.so.2")
      (load-shared-object "libloop.so")))
  

  (define mutex (make-mutex))

  (define pk
    (lambda args
      (display ";; ")(write args)(newline)
      (flush-output-port)
      (car (reverse args))))

  (define entangle-current (make-parameter #f))
 
  (define prompt-current #f)

  (define EWOULDBLOCK 11)

  (define prompt-singleton '(prompt-singleton))

  (define-record-type* <entangle>
    (make-entangle-base running epoll events thunks others readable writable)
    entangle?
    (running entangle-running? entangle-running!)
    (epoll entangle-epoll)
    (events entangle-events)
    (thunks entangle-thunks entangle-thunks!)
    (others entangle-others entangle-others!)
    (readable entangle-readable)
    (writable entangle-writable))
  
  (define (call-with-prompt thunk handler)
    (call-with-values (lambda ()
                        (call/1cc
                         (lambda (k)
                           ;; XXX: The continuation K also called
                           ;; prompt-current may be called in THUNK during
                           ;; the extent of this lambda.
                           (set! prompt-current k)
                           (thunk))))
      (lambda out
        (cond
         ((and (pair? out) (eq? (car out) prompt-singleton))
          (apply handler (cdr out)))
         (else (apply values out))))))

  (define (entangle-abort . args)
    (call/cc
     (lambda (k)
       ;; XXX: Capture the continuation and call it later, hence
       ;; call/cc instead of call/1cc.
       (let ((prompt prompt-current))
         (set! prompt-current #f)
         (apply prompt (cons prompt-singleton (cons k args)))))))

  (define make-event cons)
  (define event-continuation car)
  (define event-mode cdr)

  (define entangle-apply
    (lambda (entangle thunk)
      (call-with-prompt
          thunk
        (lambda (k handler)
          (handler k)))))

  (define hashtable-empty?
    (lambda (h)
      (fx=? (hashtable-size h) 0)))
  
  (define entangle-run-once
    (lambda (entangle)
      (for-each (lambda (thunk)
                  (entangle-apply entangle thunk))
                (entangle-thunks entangle))
      (entangle-thunks! entangle '())
      (unless (hashtable-empty? (entangle-events entangle))
        ;; Wait for ONE event... TODO: do more.
        (let* ((event (make-epoll-event))
               ;; TODO: increase max events from 1 to 1024, instead of -1
               ;; adjust timeout to the next scheduled event
               (count (epoll-wait (entangle-epoll (entangle-current)) event 1 -1))
               (mode (if (epoll-event-in? event) 'read 'write))
               (k (hashtable-ref (entangle-events entangle)
                                 (cons (epoll-event-fd event) mode)
                                 #f)))
          (foreign-free (ftype-pointer-address event))
          (hashtable-delete! (entangle-events entangle) event)
          ;; remove the associated event mode from epoll instance
          (entangle-apply entangle k)))))

  (define entangle-watcher
    (lambda ()
      (entangle-read (entangle-readable (entangle-current)))
      (let ((new
             (with-mutex mutex
               (let ((new (entangle-others (entangle-current))))
                 (entangle-others! (entangle-current) '())
                 new))))
        (entangle-thunks! (entangle-current)
                          (append new
                                  (entangle-thunks
                                   (entangle-current)))))
      (entangle-watcher)))

  (define entangle-stop
    (lambda ()
      (entangle-running! (entangle-current) #f)))
  
  (define entangle-run
    (lambda ()
      (entangle-spawn entangle-watcher)
      (let loop ()
        (when (entangle-running? (entangle-current))
          (entangle-run-once (entangle-current))
          (loop)))))
  
  (define entangle-spawn
    (lambda (thunk)
      (entangle-thunks! (entangle-current)
                        (cons thunk (entangle-thunks (entangle-current))))))

  (define entangle-sleep)
  
  (define entangle-spawn-threadsafe
    (lambda (thunk)
      (with-mutex mutex
        (entangle-others! (entangle-current)
                          (cons thunk (entangle-others (entangle-current)))))
      (entangle-write (entangle-writable (entangle-current))
                      (bytevector 20 06))))

  (define fcntl!
    (let ((func (foreign-procedure "fcntl" (int int int) int)))
      (lambda (fd command value)
        (func fd command value))))

  (define fcntl
    (let ((func (foreign-procedure "fcntl" (int int) int)))
      (lambda (fd)
        (func fd F_GETFL))))

  (define-ftype <pipe>
    (array 2 int))

  (define F_GETFL 3)
  (define F_SETFL 4)
  (define O_NONBLOCK 2048)
  
  (define entangle-nonblock!
    (lambda (fd)
      (fcntl! fd F_SETFL
              (fxlogior O_NONBLOCK
                        (fcntl fd)))))

  (define make-pipe
    (let ((func (foreign-procedure "pipe" (void* int) int)))
      (lambda ()
        (define pointer (foreign-alloc (ftype-sizeof <pipe>)))
        (call/errno* (lambda () (func pointer 0))
          (lambda (out errno)
            (when (fx=? out -1)
              (error '(letloop entangle) (strerror errno) errno))))
        (let  ((pipe (make-ftype-pointer <pipe> pointer)))
          (values (ftype-ref <pipe> (0) pipe) ;; readable
                  (ftype-ref <pipe> (1) pipe)))))) ;; writable

  (define make-entangle
    (lambda ()
      (unless (entangle-current)
        (call-with-values make-pipe
          (lambda (readable writable)
            (entangle-nonblock! readable)
            (entangle-nonblock! writable)          
            (let ((epoll (epoll-create1 0))
                  (events (make-hashtable equal-hash equal?)))
              (entangle-current (make-entangle-base #t
                                                    epoll
                                                    events
                                                    '()
                                                    '()
                                                    readable
                                                    writable))))))
      (entangle-current)))

  (define entangle-socket
    (let ((entangle-socket
           (foreign-procedure "libloop_socket" (int int int) int)))
      (lambda (domain type protocol)
        (entangle-socket domain type protocol))))

  (define entangle-accept-base
    (let ((entangle-accept (foreign-procedure "accept4" (int void* void* int) int)))
      (lambda (fd)
        (entangle-accept fd 0 0 2048))))

  (define entangle-update-epoll
    (lambda (fd mode)
      (if (hashtable-ref (entangle-events (entangle-current))
                         (cons fd (if (eq? mode 'read) 'write 'read))
                         #f)
          (epoll-ctl (entangle-epoll (entangle-current))
                     3
                     fd
                     (make-epoll-event-out fd)))
          (epoll-ctl (entangle-epoll (entangle-current))
                     2
                     fd
                     (make-epoll-event-out fd))))
      
  
  (define entangle-accept
    (lambda (fd)

      (define accept-handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'read)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     1
                     fd
                     (make-epoll-event-in fd))))

      (let loop ()
        (call/errno* (lambda () (entangle-accept-base fd))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort accept-handler)
                      (entangle-update-epoll fd 'read)
                      (loop))
                    #f)
                out))))))
  
  (define entangle-close
    (let ((entangle-close (foreign-procedure "close" (int) int)))
      (lambda (fd)
        (entangle-close fd))))

  (define entangle-bind
    (let ((entangle-bind
           (foreign-procedure "libloop_socket_bind4" (int string int) int)))
      (lambda (fd ip port)
        (entangle-bind fd ip port))))

  (define entangle-listen
    (let ((entangle-listen (foreign-procedure "listen" (int int) int)))
      (lambda (fd backlog)
        (entangle-listen fd backlog))))

  (define entangle-read-base
    (let ((entangle-read
           (foreign-procedure "read" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
          (entangle-read fd
                         (bytevector-pointer bytevector)
                         (bytevector-length bytevector))))))

  (define entangle-read
    (lambda (fd)
      (define bv (make-bytevector 1024))
      
      (define handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'read)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     1
                     fd
                     (make-epoll-event-in fd))))
      
      (let loop ()
        (call/errno* (lambda () (entangle-read-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort handler)
                      (loop))
                    #f)
                (subbytevector bv 0 out)))))))
                
  
  (define entangle-write-base
    (let ((entangle-write
           (foreign-procedure "write" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
          (entangle-write fd
                          (bytevector-pointer bytevector)
                          (bytevector-length bytevector))))))

  (define entangle-write
    (lambda (fd bv)
      (define handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'write)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     3 ;; EPOLL_CTL_MOD
                     fd
                     (make-epoll-event-out fd))))
      
      (let loop ()
        (call/errno* (lambda () (entangle-write-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort handler)
                      (loop))
                    #f)
                ;; TODO: continue writing until everything is written
                #t))))))
  
  (define subbytevector
    (case-lambda
     ((bv start end)
      (assert (bytevector? bv))
      (unless (<= 0 start end (bytevector-length bv))
        (error 'subbytevector "Invalid indices" bv start end))
      (if (and (fxzero? start)
               (fx=? end (bytevector-length bv)))
          bv
          (let ((ret (make-bytevector (fx- end start))))
            (bytevector-copy! bv start
                              ret 0 (fx- end start))
            ret)))
     ((bv start)
      (subbytevector bv start (bytevector-length bv)))))

  (define entangle-tcp-serve
    (lambda (ip port)
   
      (define SOCKET-DOMAIN=AF-INET 2)
      (define SOCKET-TYPE=STREAM 1)      
      (define fd (entangle-socket SOCKET-DOMAIN=AF-INET SOCKET-TYPE=STREAM 0))

      (define close (lambda () (entangle-close fd)))

      (define read
        (lambda ()
          (let ((client (entangle-accept fd)))
            (if client
                (values (lambda () (entangle-read client))
                        (lambda (bv) (entangle-write client bv))
                        (lambda () (entangle-close client)))
                (begin
                  (values #f #f #f))))))

      
      (entangle-bind fd ip port)
      (entangle-listen fd 128)

      (values read close))) 
  )
