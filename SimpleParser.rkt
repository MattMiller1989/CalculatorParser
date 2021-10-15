#lang racket

(require racket/trace)
;(define file1 "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input01.txt" )
;(define file2 "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input02.txt" )
;(define file3 "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input03.txt" )
;(define file4 "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input04.txt" )
;(define file5 "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input05.txt" )
;(define file6 "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input06.txt" )

(define file1 "Input01.txt" )
(define file2 "Input02.txt" )
(define file3 "Input03.txt" )
(define file4 "Input04.txt" )
(define file5 "Input05.txt" )
(define file6 "Input06.txt" )

(define(parse inputFile)
  ;(display (scan(remove-white-space (scan-prep inputFile) ) 1))
  (define tokens (scan(remove-white-space (scan-prep inputFile) ) 1))
  (define token-stream (clean-tokens tokens))
  ;(display token-stream)
  (cond
    [(string-contains? tokens "Error") tokens]
    [else (program token-stream 1)]
  )
  
  )

(define (scan-prep inputFile)
  
  (port->string (open-input-file inputFile) #:close? #t)
  
  )

;https://stackoverflow.com/questions/34981123/how-to-parse-this-string-in-scheme
(define (remove-white-space file-contents)  
  (string-split (string-replace(string-replace (string-replace (string-replace file-contents "(" "( " )  ")" " )" ) "\n" "newLine ") ":=" "=="))
 )
(define (clean-tokens token-stream)  
   (string-split token-stream)
  )


(define (scan element-list line)  
  
  (cond
    [(string=? "$$" (first element-list)) "$$\nDone-Scanning\n"]
    [(string=? "newLine" (first element-list)) (string-append "newLine" " " (scan (rest element-list) (+ line 1)))]
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
    [(is-mult-op? element) "multiply"]
    [(is-div-op? element) "divide"]
    [(is-add-op? element) "add"]
    [(is-minus-op? element) "minus"]
    [(is-assign? element) "assign"]
    [else (error (string-append "Scan Error on line " (~a line)))]
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
  (regexp-match? #rx"(^\\*$)" element)
  )
(define (is-div-op? element)
  (regexp-match? #rx"(^/$)" element)
  )

(define (is-add-op? element)
  (regexp-match? #rx"(^\\+$)" element)
  )
(define (is-minus-op? element)
  (regexp-match? #rx"(^-$)" element)
  )

(define (is-assign? element)
  (regexp-match? #rx"^(==)$" element)
  )
  


(define (program token-stream line)
  (define curr-token (first token-stream))
  (cond
    [(string=? curr-token "id") (success (match (stmt_list token-stream line) "$$"))]
    [(string=? curr-token "read") (success (match (stmt_list token-stream line) "$$"))]
    [(string=? curr-token "write") (success (match (stmt_list token-stream line) "$$"))]
    [(string=? curr-token "$$") (success (match (stmt_list token-stream line) "$$"))]
    [else (error(string-append "ERROR ON LINE" (~a line)))]
    )
  )

(define (stmt_list token-stream line)
  (define curr-token (first token-stream))
    (cond
    [(string=? curr-token "id") (stmt_list (stmt token-stream line) line)  ]
    [(string=? curr-token "read") (stmt_list (stmt token-stream line) line ) ]
    [(string=? curr-token "write") (stmt_list (stmt token-stream line) line ) ]
    [(string=? curr-token "$$") token-stream]
    [(string=? curr-token "newLine") (stmt_list (rest token-stream)  (+ 1 line))]
    [else (error(string-append "ERROR ON LINE " (~a line))) ]
    ))



(define (stmt token-stream line)
  (define curr-token (first token-stream)) 
  (cond
    [(string=? curr-token "id")(expr (match (match token-stream "id") "assign") line)]
    [(string=? curr-token "read") (match (match token-stream "read" ) "id" )]
    [(string=? curr-token "write") (expr (match token-stream "write" ) line)]
    [(string=? curr-token "newLine") (stmt (rest token-stream) (+ 1 line))]
    [else (error(string-append "ERROR ON LINE " (~a line)))]
    ))

(define (expr token-stream line)
  (define curr-token (first token-stream))  
  (cond
    [(string=? curr-token "lParen") (term_tail (term token-stream line) line)]
    [(string=? curr-token "id") (term_tail (term token-stream line) line)]
    [(string=? curr-token "num") (term_tail (term token-stream line) line)]
    [(string=? curr-token "newLine") (expr (rest token-stream) (+ 1 line))]
    [else (error(string-append "ERROR ON LINE " (~a line)))]
    ))

(define (term_tail token-stream line)
  (define curr-token (first token-stream))  
  (cond
    [(string=? curr-token "add") (term_tail (term (add_op token-stream line) line) line)]
    [(string=? curr-token "minus") (term_tail (term (add_op token-stream line) line) line)]
    [(string=? curr-token "write") token-stream]
    [(string=? curr-token "read") token-stream]
    [(string=? curr-token "$$") token-stream]
    [(string=? curr-token "id") token-stream]
    [(string=? curr-token "rParen") token-stream]
    [(string=? curr-token "newLine") (term_tail (rest token-stream) (+ 1 line))]
    [else (error(string-append "ERROR ON LINE " (~a line)))]
    ))

(define (term token-stream line)
  (define curr-token (first token-stream))
  
  (cond
    [(string=? curr-token "id") (factor_tail (factor token-stream line) line)]
    [(string=? curr-token "num") (factor_tail (factor token-stream line) line)]
    [(string=? curr-token "lParen") (factor_tail (factor token-stream line) line)]
    [(string=? curr-token "newLine") (term (rest token-stream)(+ 1 line))]
    [else (error(string-append "ERROR ON LINE " (~a line)))]
    ))

(define (factor_tail token-stream line)
  (define curr-token (first token-stream))  
  (cond
    [(string=? curr-token "multiply") (factor_tail (factor (mult_op token-stream line) line) line)]
    [(string=? curr-token "divide") (factor_tail (factor (mult_op token-stream line) line) line)]
    [(string=? curr-token "add") token-stream]
    [(string=? curr-token "minus") token-stream]
    [(string=? curr-token "$$") token-stream]
    [(string=? curr-token "id") token-stream]
    [(string=? curr-token "rParen") token-stream]
    [(string=? curr-token "read") token-stream]
    [(string=? curr-token "write") token-stream]
    [(string=? curr-token "newLine") (factor_tail (rest token-stream)(+ 1 line))]
    [else (error(string-append "ERROR ON LINE " (~a line)))]
    ))

(define (factor token-stream line)
  (define curr-token (first token-stream)) 
  (cond
    [(string=? curr-token "id")(match token-stream "id" )]
    [(string=? curr-token "num") (match token-stream "num" )]
    [(string=? curr-token "lParen") (match (expr (match token-stream "lParen") line) "rParen" )]
    [(string=? curr-token "newLine") (factor (rest token-stream) (+ 1 line))]
    [else (error(string-append "ERROR ON LINE " (~a line)))]
    ))

(define (add_op token-stream line)
  (define curr-token (first token-stream))  
  (cond
    [(string=? curr-token "add")(match token-stream "add" )]
    [(string=? curr-token "minus")(match token-stream "minus" )]
    [else (error(string-append "ERROR ON LINE " (~a line))) ]
    ))

(define (mult_op token-stream line)
  (define curr-token (first token-stream)) 
  (cond
    [(string=? curr-token "multiply")(match token-stream "multiply" )]
    [(string=? curr-token "divide") (match token-stream "divide" )]
    [else (error(string-append "ERROR ON LINE " (~a line)))]
    ))
  
(define (match token-stream token )
  
  (cond
    [(string=? (first token-stream) "ERROR") '("ERROR",token)]
   ; [(string=? "$$" token) (exit)]
    [(string=? (first token-stream) token) (rest token-stream)]    
    [else (error "ERROR ON LINE " )]
    ))

(define (success token-stream)
  (cond
    [(string=? (first token-stream) "Done-Scanning") "Successfuly Parsed!!"]
    [else (first token-stream)]
    ))