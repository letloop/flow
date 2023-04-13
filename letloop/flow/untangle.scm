(library (letloop flow untangle)

  (export make-untangle
          #;untangle-abort
          untangle-run
          #;untangle-sleep
          untangle-spawn
          #;untangle-spawn-threadsafe
          #;untangle-stop
          #;untangle-tcp-connect
          untangle-tcp-serve
          )

  (import (chezscheme)
          (letloop r999) (letloop cffi) (letloop liburing))
 
  #;(define untangle-time
    (lambda ()
      (current-jiffy)))

  (define untangle-log
    (lambda (level message . objects)
      (unless (eq? level 'debug)
        (format (current-error-port) "~a: ~a ~a\n" level message objects)
        (void))
      (void)))

  (define mutex (make-mutex))
  
  (define pk
    (lambda args
      (when (getenv "LETLOOP_DEBUG")
        (display ";;; ")
        (write args)
        (newline)
        (flush-output-port))
      (car (reverse args))))

  (define untangle-singleton '(untangle-singleton))

  (define current-prompt #f)

  (define (call-with-prompt thunk handler)
    (call-with-values (lambda ()
                        (call/1cc
                         (lambda (k)
                           ;; XXX: The continuation K also called
                           ;; current-prompt may be called in THUNK during
                           ;; the extent of this lambda.
                           (set! current-prompt k)
                           (thunk))))
      (lambda out
        (cond
         ((and (pair? out) (eq? (car out) untangle-singleton))
          (apply handler (cdr out)))
         (else (apply values out))))))

  (define (abort-to-prompt . args)
    (assert current-prompt)
    (call/cc
     (lambda (k)
       (let ((prompt current-prompt))
         (set! current-prompt #f)
         (apply prompt (cons untangle-singleton (cons k args)))))))

  (define-record-type* <untangle>
    (make-untangle% running io-uring continuations cqe todos aliens readable writable)
    untangle?
    (running untangle-running untangle-running!)
    (io-uring untangle-io-uring)
    (continuations untangle-continuations)
    (cqe untangle-cqe)
    (todos untangle-todos-ref untangle-todos-set!)
    (aliens untangle-aliens)
    (readable untangle-readable)
    (writable untangle-writable))
  
  (define untangle-stats
    (lambda ()
      (define untangle (untangle-current))
      (list 'running (untangle-running untangle)
            'continuations (hashtable-size (untangle-continuations untangle))
            'continuations (hashtable-values (untangle-continuations untangle))
            'todos (length (untangle-todos-ref untangle))
            'aliens (length (unbox (untangle-aliens untangle))))))
  
  (define-ftype <pipe>
    (array 2 int))

  (define fcntl
    (let ((func (foreign-procedure "fcntl" (int int int) int)))
      (lambda (fd command value)
        (func fd command value))))

  (define fcntl-read
    (let ((func (foreign-procedure "fcntl" (int int) int)))
      (lambda (fd)
        (func fd F_GETFL))))
  
  (define make-letloop-socket
    (let ((func (foreign-procedure "libloop_socket" (int int int) int)))
      (lambda (domain type protocol)
        (func domain type protocol))))

  (define make-pipe
    (let ((func (foreign-procedure "pipe" (void* int) int)))
      (lambda ()
        (define pointer (foreign-alloc (ftype-sizeof <pipe>)))
        (call-with-errno (lambda () (func pointer 0))
          (lambda (out errno)
            (when (fx=? out -1)
              (error '(letloop untangle) (strerror errno) errno))))
        (let  ((pipe (make-ftype-pointer <pipe> pointer)))
          (values (ftype-ref <pipe> (0) pipe) ;; readable
                  (ftype-ref <pipe> (1) pipe)))))) ;; writable

  (define untangle-current (make-parameter #f))

  (define F_GETFL 3)
  (define F_SETFL 4)
  (define O_NONBLOCK 2048)

  (define untangle-nonblock!
    (lambda (fd)
      (fcntl fd F_SETFL
             (logior O_NONBLOCK
                       (fcntl-read fd)))))
 
  (define make-untangle
    (lambda ()
      (call-with-values make-pipe
        (lambda (readable writable)
          (untangle-nonblock! readable)
          (untangle-nonblock! writable)          
          (let ((untangle (make-untangle% #t
                                          (make-io-uring)
                                          (make-hashtable equal-hash =)
                                          (foreign-alloc 8)
                                          '()
                                          (box '())
                                          readable
                                          writable)))
            (io-uring-queue-init 128 (untangle-io-uring untangle) 0)
            (untangle-current untangle)
            untangle)))))

  (define-record-type* <untangle-error>
    (make-untangle-error% symbol data)
    untangle-error?
    (symbol untangle-error-symbol)
    (data untangle-error-data))

  (define make-untangle-continuation
    (lambda (untangle type proc)
      (let loop ()
        (define uid (random (expt 2 64)))
        (if (hashtable-contains? (untangle-continuations untangle) uid)
            (loop)
            (begin
              (hashtable-set! (untangle-continuations untangle) uid (cons type proc))
              uid)))))

  (define untangle-spawn
    (case-lambda
     ((thunk) (untangle-spawn* (untangle-current) thunk))
     ((seconds thunk) (untangle-spawn* (untangle-current) seconds thunk))))
  
  (define untangle-spawn*
    (case-lambda
     ((untangle thunk)
      (untangle-todos-set! untangle (cons thunk (untangle-todos-ref untangle)))
      (untangle-write untangle (untangle-writable untangle) (bytevector 37 33)))
     ((untangle seconds thunk)

      (define seconds->timespec
        (lambda (seconds)
          (define seconds* (exact (floor seconds)))
          (define nanoseconds (exact (floor (* (- seconds seconds*) (expt 10 9)))))

          (ftype-pointer-address (make-timespec seconds* nanoseconds))))

      (let ((timespec (seconds->timespec seconds))
            (uid (make-untangle-continuation untangle 'timeout (lambda _ (thunk)))))
        (define sqe (io-uring-get-sqe (untangle-io-uring untangle)))
        (io-uring-sqe-set-data64 sqe uid)
        (io-uring-prep-timeout sqe timespec -1 0)
        (io-uring-submit (untangle-io-uring untangle))
        (foreign-free timespec)))))

  (define inc 0)
  
  (define untangle-spawn-threadsafe
    (lambda (thunk)
      (define untangle (untangle-current))
      (define aliens (untangle-aliens untangle))
      
      (let ((thunks (unbox aliens)))
        (if (box-cas! aliens thunks (cons thunk thunks))
            (untangle-write untangle (untangle-writable untangle) (bytevector 37 33))
            (untangle-spawn-threadsafe thunk)))))

  (define untangle-abort abort-to-prompt)

  #;(define untangle-sleep
    (lambda (untangle seconds)

      (define handler
        (lambda (k)
          (define seconds->timespec
            (lambda (seconds)
              (define seconds* (exact (floor seconds)))
              (define nanoseconds (exact (floor (* (- seconds seconds*)
                                                   (expt 10 9)))))

              (ftype-pointer-address (make-timespec seconds* nanoseconds))))

          (let ((timespec (seconds->timespec seconds))
                (uid (make-untangle-continuation untangle 'timeout (lambda _ (k)))))
            (define sqe (io-uring-get-sqe (untangle-io-uring untangle)))
            (io-uring-sqe-set-data64 sqe uid)
            (io-uring-prep-timeout sqe timespec -1 0)
            (io-uring-submit (untangle-io-uring untangle))
            (foreign-free timespec))))

      (abort-to-prompt handler)))

  (define untangle-close
    (let ((func (foreign-procedure "close" (int) int)))
      (lambda (fd)
        (call-with-values (lambda () (call/errno (lambda () (func fd))))
          (lambda (out errno)
            (if (fx=? out -1)
                (begin
                  (untangle-log 'error (format #f "Failed to close socket: ~a @ ~a" (strerror errno) fd))
                  #f)
                #t))))))

  (define untangle-bind
    (let ((func (foreign-procedure "libloop_socket_bind4" (int string int) int)))
      (lambda (fd ip port)
        (call-with-values (lambda () (call/errno (lambda () (func fd ip port))))
          (lambda (out errno)
            (if (fx=? out -1)
                (begin
                  (untangle-log 'error (format #f "Failed to bind socket: ~a @ ~a" (strerror errno) fd))
                  #f)
                #t))))))

  (define untangle-connect
    (let ((func (foreign-procedure "libloop_socket_connect4" (void* int string int) void)))
      (lambda (sqe fd ip port)
        (func sqe fd ip port))))

  (define untangle-listen
    (let ((func (foreign-procedure "listen" (int int) int)))
      (lambda (fd backlog)
        (call-with-values (lambda () (call/errno (lambda () (func fd backlog))))
          (lambda (out errno)
            (if (fx=? out -1)
                (begin
                  (untangle-log 'error (format #f "Failed to listen socket: ~a @ ~a" (strerror errno) fd))
                  #f)
                #t))))))

  (define untangle-accept4
    (let ((func (foreign-procedure "accept4" (int void* void* int) int)))
      (lambda (fd)
        (func fd 0 0 0))))

  (define untangle-tcp-serve
    (lambda (ip port)
      (untangle-tcp-serve* (untangle-current) ip port)))
  
  (define untangle-tcp-serve*
    (lambda (untangle ip port)

      (define handler
        (lambda (k)

          (define on-socket-ready
            (lambda (fd)
              (define open? #t)

              (define close-server-socket
                (lambda ()
                  (set! open? #f)
                  (untangle-close fd)))

              (define generate-client-connection
                (lambda ()
                  (let ((client (untangle-accept untangle fd)))
                    (if client
                        (values 
                                (make-client-reader untangle client)
                                (make-client-writer untangle client)
                                (make-client-closer client)
)
                        (begin
                          (untangle-log 'error "Accepted a buggy socket" client)
                          (values #f #f #f))))))

              (if (not fd)
                  (begin
                    (untangle-log 'error "Failed to prepare socket")
                    (k #f #f))
                  (begin
                    (untangle-nonblock! fd)
                    (untangle-bind fd ip port)
                    (untangle-listen fd 2048)
                    (pk "Socket fd bound, and listenning" fd)
                    (k generate-client-connection close-server-socket)))))

          (define sqe (io-uring-get-sqe (untangle-io-uring untangle)))

          (let ((AF_INET 2)
                (SOCK_STREAM 1)
                (uid (make-untangle-continuation untangle 'socket on-socket-ready)))
            (io-uring-sqe-set-data64 sqe uid)
            (io-uring-prep-socket sqe AF_INET SOCK_STREAM 0 0)
            (io-uring-submit (untangle-io-uring untangle)))))

      (abort-to-prompt handler)))

  (define make-client-closer
    (lambda (fd)
      (lambda ()
        (untangle-close fd))))

  (define make-client-reader
    (lambda (untangle fd)
      (lambda ()
        (untangle-read untangle fd 1024))))

  (define make-client-writer
    (lambda (untangle fd)
      (lambda (bytevector)
        (let loop ((bytevector bytevector))
          (when bytevector
            (loop (untangle-write untangle fd bytevector)))))))
  
  (define untangle-tcp-connect
    (lambda (untangle ip port)

      (define handler
        (lambda (k)

          (define on-socket-ready
            (lambda (fd)

              (define on-socket-connected
                (lambda (maybe-error)
                  (if (untangle-error? maybe-error)
                      (begin
                        (untangle-log 'error "Failed to connect socket" maybe-error)
                        (k #f #f #f))
                      (k (make-client-reader untangle fd)
                         (make-client-writer untangle fd)
                         (make-client-closer fd)))))

              (if (not fd)
                  (begin
                    (untangle-log 'error "Failed to prepare socket")
                    (k #f #f #f))
                  (let ((sqe (io-uring-get-sqe (untangle-io-uring untangle)))
                        (uid* (make-untangle-continuation untangle 'connect on-socket-connected)))
                    (untangle-nonblock! fd)
                    (io-uring-sqe-set-data64 sqe uid*)
                    (untangle-connect sqe fd ip port)
                    (io-uring-submit (untangle-io-uring untangle))))))

          (define sqe (io-uring-get-sqe (untangle-io-uring untangle)))

          (let ((AF_INET 2)
                (SOCK_STREAM 1)
                (uid (make-untangle-continuation untangle 'socket on-socket-ready)))
            (io-uring-sqe-set-data64 sqe uid)
            (io-uring-prep-socket sqe AF_INET SOCK_STREAM 0 0)
            (io-uring-submit (untangle-io-uring untangle)))))

      (abort-to-prompt handler)))

  (define untangle-accept
    (lambda (untangle fd)

      (define handler
        (lambda (k)
          (define on-client-socket
            (lambda (client)
              (if (not client)
                  (begin
                    (untangle-log 'error "Failed to accept socket" client)
                    (k #f))
                  (k client))))

          (define sqe (io-uring-get-sqe (untangle-io-uring untangle)))
          (define uid (make-untangle-continuation untangle 'accept on-client-socket))

          (io-uring-sqe-set-data64 sqe uid)
          (io-uring-prep-accept sqe fd 0 0 0)
          (io-uring-submit (untangle-io-uring untangle))))

      #;(abort-to-prompt handler)

      (call-with-values (lambda () (call/errno (lambda ()
                                                 (untangle-accept4 fd))))
        (lambda (out errno)
          (if (fx=? out -1)
              (if (fx=? errno 11)
                  (abort-to-prompt handler)
                  (begin
                    (untangle-log 'error (format #f "Failed to accept socket: ~a @ ~a" (strerror errno) fd))
                    #f))
              out)))))

  (define bytevector-empty (bytevector))

  (define untangle-write
    (lambda (untangle fd bytevector)

      (define handler
        (lambda (k bytevector)

          (define on-write
            (lambda (bytes)
              (if (or (not bytes) (fxzero? bytes))
                  (begin
                    (untangle-log 'error "Failed to write on socket")
                    (k #f))
                  (if (fx=? bytes (bytevector-length bytevector))
                      (k #f)
                      (if (fx=? bytes (bytevector-length bytevector))
                          (k #f)
                          (k (subbytevector bytevector bytes (bytevector-length bytevector))))))))

          (define sqe (io-uring-get-sqe (untangle-io-uring untangle)))
          (define uid (make-untangle-continuation untangle 'write on-write))

          (io-uring-sqe-set-data64 sqe uid)
          (with-lock (list bytevector)
            (io-uring-prep-write sqe
                                 fd
                                 (bytevector-pointer bytevector)
                                 (bytevector-length bytevector)
                                 0))
          (io-uring-submit (untangle-io-uring untangle))))

      (define write
        (let ((write* (foreign-procedure "write" (int void* size_t) ssize_t)))
          (lambda (fd bytevector)
            (assert (bytevector? bytevector))
            (with-lock (list bytevector)
              (write* fd
                      (bytevector-pointer bytevector)
                      (bytevector-length bytevector))))))

      (call-with-errno (lambda () (write fd bytevector))
        (lambda (out errno)
          (if (fx=? out -1)
              (if (fx=? errno 11)
                  (abort-to-prompt handler bytevector)
                  (begin
                    (untangle-log 'error "Failed to write on socket" fd (strerror errno))
                    #f))
              (if (fx=? out (bytevector-length bytevector))
                  #f
                  (subbytevector bytevector out (bytevector-length bytevector))))))))

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

  (define untangle-read
    (lambda (untangle fd length)

      (define handler
        (lambda (k bytevector)

          (define bytevector (make-bytevector 1024))

          (define on-read
            (lambda (bytes)
              (if (or (not bytes) (fxzero? bytes))
                  (begin
                    #;(untangle-log 'error "Failed to read on socket #0" fd bytes)
                    (k #f))
                  (k (subbytevector bytevector 0 bytes)))))

          (define sqe (io-uring-get-sqe (untangle-io-uring untangle)))
          (define uid (make-untangle-continuation untangle 'read on-read))

          (io-uring-sqe-set-data64 sqe uid)
          (with-lock (list bytevector)
            (io-uring-prep-read sqe
                                fd
                                (bytevector-pointer bytevector)
                                (bytevector-length bytevector)
                                0))
          (io-uring-submit (untangle-io-uring untangle))))

      (define read
        (let ((read* (foreign-procedure "read" (int void* size_t) ssize_t)))
          (lambda (fd bytevector)
            (with-lock (list bytevector)
              (read* fd
                     (bytevector-pointer bytevector)
                     (bytevector-length bytevector))))))

      (define bytevector (make-bytevector length))

      (call-with-errno (lambda () (read fd bytevector))
        (lambda (out errno)
          (if (fx=? out -1)
              (if (fx=? errno 11) ;; EAGAIN
                  (abort-to-prompt handler bytevector)
                  (begin
                    (untangle-log 'error "Failed to read on socket #1" fd (strerror errno))
                    #f))
              (if (fxzero? out)
                  (abort-to-prompt handler bytevector)
                  (if (fx=? out (bytevector-length bytevector))
                      bytevector
                      (subbytevector bytevector 0 out))))))))

  (define untangle-peek-one
    (lambda (untangle cqe continue?)
      (define out (io-uring-peek-cqe (untangle-io-uring untangle) cqe))
      (if (fxzero? out)
          (let* ((res (io-uring-cqe-get-res cqe))
                 (uid (io-uring-cqe-get-data64 cqe))
                 (future (hashtable-ref (untangle-continuations untangle)
                                        uid
                                        #f)))
            (hashtable-delete! (untangle-continuations untangle) uid)
            (io-uring-cqe-seen (untangle-io-uring untangle) cqe)
            (if (fxnegative? res)
                (if (and (eq? (car future) 'timeout) (fx=? res -62)) ;; -ETIME
                    (values #t 0 (car future) (cdr future))
                    (begin
                      (untangle-log 'error "Completion event error" (car future) (cdr future) (fx- res) (strerror (fx- res)))
                      (values #f #f (car future) (cdr future))))
                (values #t res (car future) (cdr future))))
          (begin
            (untangle-log 'debug "No more completion events")
            (values #f #f #f #f)))))

  (define untangle-wait-one
    (lambda (untangle cqe continue?)
      (define out (io-uring-wait-cqe (untangle-io-uring untangle) cqe))
      (if (fxzero? out)
          (let* ((res (io-uring-cqe-get-res cqe))
                 (uid (io-uring-cqe-get-data64 cqe))
                 (future (hashtable-ref (untangle-continuations untangle)
                                        uid
                                        #f)))
            (hashtable-delete! (untangle-continuations untangle) uid)
            (io-uring-cqe-seen (untangle-io-uring untangle) cqe)
            (if (fxnegative? res)
                (if (and (eq? (car future) 'timeout) (fx=? res -62)) ;; -ETIME
                    (values #t 0 (car future) (cdr future))
                    (begin
                      (untangle-log 'error "Completion event error" (car future) (cdr future) (fx- res) (strerror (fx- res)))
                      (values #f #f (car future) (cdr future))))
                (values #t res (car future) (cdr future))))
          (begin
            (untangle-log 'error "Failed while waiting for an event" (fx- out) (strerror (fx- out)))
            (values #f #f #f #f)))))

  (define untangle-apply
    (lambda (thunk)
      (guard (ex (else
                  #;(raise ex)
                  (pk 'untangle-apply 'failure thunk
                      (apply format #f
                             (condition-message ex)
                             (condition-irritants ex))))
                 #;(raise ex))
        (call-with-prompt thunk
          (lambda (k proc . args)
            (apply proc k args))))))

  (define mark 50000)
  (define iter (make-parameter 0))
  
  (define debug*
    (lambda (i . message)
      (when (< mark i)
        (pk i message))))
  
  (define untangle-run-once
    (lambda (untangle i)
      (let ((thunks (reverse (untangle-todos-ref untangle))))
        (untangle-todos-set! untangle '())
        (for-each untangle-apply thunks))
      (debug* i "todo done")
      (let ((io-uring (untangle-io-uring untangle))
            (cqe (untangle-cqe untangle)))
        ;; wait for one, then peek more if any, that is blocking
        (call-with-values (lambda () (untangle-wait-one untangle cqe io-uring-wait-cqe))
          (lambda (ok? res type proc)
            (debug* i "wait cqe done" ok?)
            (if (not ok?)
                (when proc
                  (untangle-apply (lambda () (proc res))))
                ;; otherwise wait for more...
                (let loop ((thunks (list (cons proc (lambda () (proc res))))))
                  (call-with-values (lambda () (untangle-peek-one untangle cqe io-uring-peek-cqe))
                    (lambda (ok? res type proc)
                      (if ok?
                          (loop (cons (cons proc (lambda () (proc res))) thunks))
                          (begin
                            (debug* i "exec thunks" thunks)
                            (untangle-stats)
                            (for-each untangle-apply (map cdr thunks))
                            (debug* i "exec thunks done"))))))))))))

  (define nightwatch
    (lambda (untangle)
      (guard (ex (else (pk 'nightwatch 'failed)))
        (untangle-read untangle (untangle-readable untangle) 2)
        (let retry ()
          (let* ((aliens (untangle-aliens untangle))
                 (thunks (unbox aliens)))
            (unless (null? thunks)
              (if (box-cas! aliens thunks '())
                  (untangle-todos-set! untangle
                                       (append thunks
                                               (untangle-todos-ref untangle)))
                  (begin
                    (retry))))))
        (nightwatch untangle))))

  (define untangle-stop
    (lambda ()
      (define untangle (untangle-current))
      (untangle-running! untangle #f)
      (untangle-spawn* untangle (lambda () (pk 'oops)))))

  (define untangle-run
    (lambda ()
      (define untangle (untangle-current))
      (untangle-spawn* untangle (lambda () (nightwatch untangle)))
      (let loop ()
        (when (untangle-running untangle)
          (guard (ex (else (raise ex) (pk 'oops2 (condition-message ex))))
            (untangle-run-once untangle 0))
          (loop)))))

  )
