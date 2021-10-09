#lang racket
(define file "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input01.txt" )

(define (parse inputFile)
  
  (display (scan(remove-white-space (scan-prep inputFile) ) 0))
  ;(remove-white-space (scan-prep inputFile) )
  )

(define (scan-prep inputFile)
  
  (port->string (open-input-file inputFile) #:close? #t)
  
  )

;https://stackoverflow.com/questions/34981123/how-to-parse-this-string-in-scheme
(define (remove-white-space file-contents)  
  (string-split (string-replace (string-replace file-contents "(" "( " )  ")" " )" ))
 )

(define (scan element-list line)
  
  (if (string=? "$$" (first element-list))
       "\nDone Scanning" 
      (string-append (process (first element-list) line) " " (scan (rest element-list) line)))
  )

(define (process element line)
  (cond
    [(is-read? element) element]
    [(is-write? element) element]
    [(is-num? element) "num"]
    [else element]
  ))

(define (is-read? element)
  (regexp-match? #rx"^(read)$" element)
  )

(define (is-write? element)
  (regexp-match? #rx"^(write)$" element)
  )

(define (is-num? element)
  (regexp-match? #rx"^[0-9.]+$" element)
  )
  
  
  
  
  
                          