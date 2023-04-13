#!chezscheme
(library (letloop cffi)
  (export call/errno call/errno* call-with-errno with-lock strerror bytevector-pointer)
  (import (chezscheme))

  ;; Chez GC must be disabled or it could stomp on errno.
  ;;
  ;; See:
  ;;
  ;;   https://github.com/cisco/ChezScheme/issues/550
  ;;
  
  (define-syntax call/errno
    (syntax-rules ()
      [(_ thunk)
       (with-interrupts-disabled
        (let ([out (thunk)])
          ;; Cache ASAP!
          (let ((errno (#%$errno)))
            (values out errno))))]))

  (define-syntax call/errno*
    (syntax-rules ()
      [(_ thunk proc)
       (with-interrupts-disabled
        (let ([out (thunk)])
          ;; Cache ASAP!
          (let ((errno (#%$errno)))
            (proc out errno))))]))

  (define-syntax call-with-errno
    (syntax-rules ()
      ((_ thunk proc)
       (let ((out #f)
             (errno #f))
         (with-interrupts-disabled
          (set! out (thunk))
          (set! errno (#%$errno)))
         (proc out errno)))))

  (define-syntax with-lock
    (syntax-rules ()
      ((_ objects body ...)
       (let ((objects* objects))
         (for-each lock-object objects*)
         (call-with-values (lambda () body ...)
           (lambda out
             (for-each unlock-object objects*)
             (apply values out)))))))

  (define (bytevector-pointer bv)
    ;; TODO: understand what this is
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  (define stdlib (load-shared-object #f))

  (define strerror
    (let ((func (foreign-procedure "strerror" (int) string)))
      (lambda (code)
        (func code))))
  )
