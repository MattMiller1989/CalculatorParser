#lang racket
(define file "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input01.txt" )

(define (parse inputFile)
  
  (display (scan(remove-white-space (scan-prep inputFile) ) 1))
  ;(remove-white-space (scan-prep inputFile) )
  )

(define (scan-prep inputFile)
  
  (port->string (open-input-file inputFile) #:close? #t)
  
  )

;https://stackoverflow.com/questions/34981123/how-to-parse-this-string-in-scheme
(define (remove-white-space file-contents)  
  (string-split (string-replace (string-replace (string-replace file-contents "(" "( " )  ")" " )" ) "\n" "newLine "))
 )

(define (scan element-list line)
  
  ;(if (string=? "$$" (first element-list))
   ;    "\nDone Scanning" 
    ;  ((if (string=? "newLine" (first element-list))
     ; (scan (rest element-list) (+ line 1))
      ; (string-append (process (first element-list) line) " " (scan (rest element-list) line))))
  ;)
  (cond
    [(string=? "$$" (first element-list)) "\nDone Scanning\n"]
    [(string=? "newLine" (first element-list)) (scan (rest element-list) (+ line 1))]
    ;[else (string-append (process (first element-list) line) " " (scan (rest element-list) line))]
    [(string-contains? (process (first element-list) line) "Error") (process (first element-list) line)]
    [else (string-append (process (first element-list) line) " " (scan (rest element-list) line))]
  ))

(define (process element line)
  (cond
    [(is-read? element) element ]
    [(is-write? element) element]
    [(is-num? element) "num"]
    [(is-id? element) "id"]
    [(is-l-paren? element) "lParen"]
    [(is-r-paren? element) "rParen"]
    [(is-mult-op? element) "multOp"]
    [(is-add-op? element) "addOp"]   
    [else (string-append "\nError on line " (~a line))]
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

;https://stackoverflow.com/questions/17564088/how-to-form-a-regex-to-recognize-correct-declaration-of-variable-names/17564142
(define (is-id? element)
  (regexp-match? #rx"^[a-zA-Z_][a-zA-Z_0-9]*$" element)
  )

(define (is-l-paren? element)
  (regexp-match? #rx"^\\($" element)
  )

(define (is-r-paren? element)
  (regexp-match? #rx"^\\)$" element)
  )

(define (is-mult-op? element)
  (regexp-match? #rx"(^\\*$)|(^/$)" element)
  )

(define (is-add-op? element)
  (regexp-match? #rx"(^\\+$)|(^-$)" element)
  )
  
  
  
  
  
                          