#lang racket
(define file "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input01.txt" )

(define (parse inputFile)
  
  (scan(remove-white-space (scan-prep inputFile) ))
  )

(define (scan-prep inputFile)
  
  (port->string (open-input-file inputFile) #:close? #t)
  
  )

;https://stackoverflow.com/questions/34981123/how-to-parse-this-string-in-scheme
(define (remove-white-space file-contents)
  
  (string-split file-contents)  
 )

(define (scan element-list)
  
  (if (empty? element-list)
      (write "Done Scanning \n" )
      (process element-list)
  ))

(define (process element-list)
  (display (first element-list))
  (display " ")
  (scan (rest element-list))
  )
  
  
  
                          