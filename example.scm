(import (chezscheme) (letloop flow entangle))

(define pk
  (lambda args
    (display ";; ")(write args)(newline)
    (flush-output-port)
    (car (reverse args))))

(define response (string->utf8 "HTTP/1.1 200 OK\r
Content-Length: 14\r
Connection: Closed\r
\r
Hello schemer
"))

(define client-handle
  (lambda (read write close)
    (read)
    (write response)
    (close)))

(define main
  (lambda ()
    (call-with-values (lambda () (entangle-tcp-serve ip port))
      (lambda (accept close)
        (format #t "HTTP server running at http://~a:~a\n" ip port)
        (let loop ()
          (call-with-values accept
            (lambda (read write close)
              (entangle-spawn 
               (lambda ()
                 (client-handle read write close)))))
          (loop))))))

(define ip (cadr (command-line)))
(define port (string->number (caddr (command-line))))

(make-entangle)
(entangle-spawn main)
(entangle-run)
