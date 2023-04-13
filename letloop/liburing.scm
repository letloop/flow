(library (letloop liburing)
  (export
   make-timespec
   make-io-uring
   io-uring-cqe-get-data64
   io-uring-cqe-get-res
   io-uring-cqe-seen
   io-uring-get-sqe
   io-uring-prep-accept
   io-uring-prep-connect
   io-uring-prep-read
   io-uring-prep-recv
   io-uring-prep-recvmsg
   io-uring-prep-send
   io-uring-prep-sendmsg
   io-uring-prep-socket
   io-uring-prep-sync-file-range
   io-uring-prep-timeout
   io-uring-prep-write
   io-uring-prep-cancel64
   io-uring-queue-exit
   io-uring-queue-init
   io-uring-sqe-set-data64
   io-uring-sqe-set-flags
   io-uring-submit
   io-uring-wait-cqe
   io-uring-peek-cqe
   )
  (import (chezscheme))

  ;; helpers

  (define-ftype <kernel-timespec>
    (struct
     (seconds long-long)
     (nanoseconds long-long)))

  (define make-timespec
    (lambda (seconds nanoseconds)
      ;; TODO: foreign-free
      (define out (make-ftype-pointer
                   <kernel-timespec>
                   (foreign-alloc (ftype-sizeof <kernel-timespec>))))
      (ftype-set! <kernel-timespec> (seconds) out seconds)
      (ftype-set! <kernel-timespec> (nanoseconds) out nanoseconds)
      out))

  ;; bindings

  (define liburing
    (let ()
      (load-shared-object #f)
      (load-shared-object "liburing.so.2")
      (load-shared-object "libloop.so")))

  ;; (define io-uring-sqe-size 64)
  ;; (define io-uring-cqe-size 16)
  (define io-uring-size 216)

  (define pk
    (lambda args
      (display ";; ") (write args)
      (flush-output-port)
      (car (reverse args))))

  (define make-io-uring
    (lambda ()
      (foreign-alloc io-uring-size)))

  (define io-uring-queue-init
    (let ((func (foreign-procedure "io_uring_queue_init" (unsigned void* void*) int)))
      (lambda (entries io-uring flags)
        (func entries io-uring flags))))

  (define io-uring-get-sqe
    (let ((func (foreign-procedure "io_uring_get_sqe" (void*) void*)))
      (lambda (io-uring)
        (func io-uring))))

  (define io-uring-queue-exit
    (let ((func (foreign-procedure "io_uring_queue_exit" (void*) int)))
      (lambda (io-uring)
        (func io-uring))))

  (define io-uring-submit
    (let ((func (foreign-procedure "io_uring_submit" (void*) int)))
      (lambda (io-uring)
        (let ((out (func io-uring)))
          (if (negative? out)
              (pk 'submit out)
              out)))))

  (define io-uring-sqe-set-data64
    (let ((func (foreign-procedure "libloop_io_uring_sqe_set_data64"
                                   (void* unsigned-64) void)))
      (lambda (sqe data)
        (func sqe data))))

  (define io-uring-cqe-get-data64
    (let ((func (foreign-procedure "libloop_io_uring_cqe_get_data64"
                                   (void*) unsigned-64)))
      (lambda (sqe)
        (func sqe))))

  (define io-uring-cqe-get-res
    (let ((func (foreign-procedure "libloop_io_uring_cqe_get_res"
                                   (void*) integer-32)))
      (lambda (sqe)
        (func sqe))))

  (define io-uring-sqe-set-flags
    (let ((func (foreign-procedure "libloop_io_uring_sqe_set_flags" (void* unsigned) void)))
      (lambda (sqe flags)
        (func sqe flags))))

  (define io-uring-wait-cqe
    (let ((func (foreign-procedure "libloop_io_uring_wait_cqe" (void* void*) int)))
      (lambda (io-uring cqe)
        (func io-uring cqe))))

  (define io-uring-peek-cqe
    (let ((func (foreign-procedure "libloop_io_uring_peek_cqe" (void* void*) int)))
      (lambda (io-uring cqe)
        (func io-uring cqe))))

  (define io-uring-cqe-seen
    (let ((func (foreign-procedure "libloop_io_uring_cqe_seen" (void* void*) int)))
      (lambda (io-uring cqe)
        (func io-uring cqe))))

  (define io-uring-prep-socket
    (let ((func (foreign-procedure "libloop_io_uring_prep_socket"
                                   (void* int int int unsigned) void)))
      (lambda (sqe domain type protocol flags)
        (func sqe domain type protocol flags))))

  (define io-uring-prep-connect
    (let ((func (foreign-procedure "libloop_io_uring_prep_connect"
                                   (void* int void* unsigned) void)))
      (lambda (sqe fd ip port)
        (func sqe fd ip port))))

  (define io-uring-prep-shutdown
    (let ((func (foreign-procedure "libloop_io_uring_prep_shutdown"
                                   (int int int) void)))
      (lambda (sqe fd how)
        (func sqe fd how))))

  (define io-uring-prep-timeout
    (let ((func (foreign-procedure "libloop_io_uring_prep_timeout"
                                   (void* void* unsigned unsigned) void)))
      (lambda (sqe timespec count flags)
        (func sqe timespec count flags))))

  (define io-uring-prep-read
    (let ((func (foreign-procedure "libloop_io_uring_prep_read"
                                   (void* int void* unsigned unsigned-64) void)))
      (lambda (sqe fd buffer nbytes offset)
        (func sqe fd buffer nbytes offset))))

  (define io-uring-prep-write
    (let ((func (foreign-procedure "libloop_io_uring_prep_write"
                                   (void* int void* unsigned unsigned-64) void)))
      (lambda (sqe fd buffer nbytes offset)
        (func sqe fd buffer nbytes offset))))

  (define io-uring-prep-send
    (let ((func (foreign-procedure "libloop_io_uring_prep_send"
                                   (void* int void* size_t int) void)))
      (lambda (sqe sockfd buffer length flags)
        (func sqe sockfd buffer length flags))))

  (define io-uring-prep-recv
    (let ((func (foreign-procedure "libloop_io_uring_prep_recv"
                                   (void* int void* size_t int) void)))
      (lambda (sqe sockfd buffer length flags)
        (func sqe sockfd buffer length flags))))

  (define io-uring-prep-sync-file-range
    (let ((func (foreign-procedure "libloop_io_uring_prep_sync_file_range"
                                   (void* int unsigned unsigned-64 int) void)))
      (lambda (sqe fd len offset flags)
        (func sqe fd len offset flags))))

  (define io-uring-prep-accept
    (let ((func (foreign-procedure "libloop_io_uring_prep_accept"
                                   (void* int void* void* int) void)))
      (lambda (sqe fd addr addrlen flags)
        (func sqe fd addr addrlen flags))))

  (define io-uring-prep-sendmsg
    (let ((func (foreign-procedure "libloop_io_uring_prep_sendmsg"
                                   (void* int void* unsigned) void)))
      (lambda (sqe fd message flags)
        (func sqe fd message flags))))

  (define io-uring-prep-recvmsg
    (let ((func (foreign-procedure "libloop_io_uring_prep_recvmsg"
                                   (void* int void* unsigned) void)))
      (lambda (sqe fd message flags)
        (func sqe fd message flags))))

  (define io-uring-prep-cancel64
    (let ((func (foreign-procedure "libloop_io_uring_prep_cancel64"
                                   (void* unsigned-64 int) void)))
      (lambda (sqe data flags)
        (func sqe data flags)))))
