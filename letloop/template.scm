#!chezscheme
(library (letloop template)
  (export ~check-template-000)
  (import (chezscheme))

  (define ~check-template-000
    (lambda ()
      (assert #t))))
