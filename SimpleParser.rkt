#lang racket

(define (parse inputFile)
  (scan inputFile )
  )

(define (scan inputFile)
  (define file-contents
    (port->string (open-input-file inputFile) #:close? #t))
  write file-contents
  
  )

