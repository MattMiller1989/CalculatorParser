 #lang racket
(define file "C:/Users/Matt/Desktop/Fall21/CS441/ParserProject/ParserInputFiles/Input02.txt" )

(define (parse inputFile)  
  (display (scan(remove-white-space (scan-prep inputFile) ) 1))
  (define tokens (scan(remove-white-space (scan-prep inputFile) ) 1))
  (define token-stream (clean-tokens tokens))
  (cond
    [(string-contains? tokens "Error") tokens]
    [else (parse-token token-stream (list "program") 1)]
  )
  
  )

(define (scan-prep inputFile)
  
  (port->string (open-input-file inputFile) #:close? #t)
  
  )

;https://stackoverflow.com/questions/34981123/how-to-parse-this-string-in-scheme
(define (remove-white-space file-contents)  
  (string-split (string-replace (string-replace (string-replace file-contents "(" "( " )  ")" " )" ) "\n" "newLine "))
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
    [else (string-append "\n Scan Error on line " (~a line))]
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
  
(define (parse-token token-stream call-stack line )
  (define top-of-stack (first (call-stack)))
  (cond
    [(string=? top-of-stack "ERROR") (string-append "\n Parse Error on line " (~a line))]
    [(string=? top-of-stack "program") (program token-stream line)]
    [(string=? top-of-stack "stmt_list") (stmt_list token-stream line)]
    [(string=? top-of-stack "stmt") (stmt token-stream line)]
    [(string=? top-of-stack "expr") (expr token-stream line)]
    [(string=? top-of-stack "term_tail") (term_tail token-stream line)]
    [(string=? top-of-stack "done") "\n file parsed successfully"]
    [else `("this sucks")]
    )
)

(define (program token-stream call-stack line)
  (define curr-token (first token-stream))
  (define top-of-stack (first (call-stack)))
  (define popped-stack (rest call-stack))
  (cond
    [(string=? curr-token "id") (match (parse-token token-stream (append `("stmt_list") popped-stack ) line) "$$")]
    [(string=? curr-token "read") (match (parse-token token-stream (append `("stmt_list") popped-stack ) line) "$$")]
    [(string=? curr-token "write") (match (parse-token token-stream (append `("stmt_list") popped-stack ) line) "$$")]
    [(string=? curr-token "$$") (match (parse-token token-stream (append popped-stack `("stmt_list")) line) "$$")]
    [else (parse-token (rest token-stream) `("ERROR") line)]
    )

  )

(define (stmt_list token-stream call-stack line)
  (define curr-token (first token-stream))
  (define top-of-stack (first (call-stack)))
  (define popped-stack (rest call-stack))
  (cond
    [(string=? curr-token "id") (match (parse-token token-stream (append `("stmt" "stmt_list") popped-stack ) line) "$$")]
    [(string=? curr-token "read") (match (parse-token token-stream (append `("stmt" "stmt_list") popped-stack ) line) "$$")]
    [(string=? curr-token "write") (match (parse-token token-stream (append `("stmt" "stmt_list") popped-stack ) line) "$$")]
    [(string=? curr-token "$$") (parse-token (rest token-stream) "done" line)]
    [else (parse-token (rest token-stream) "ERROR" line)]
    ))

(define (stmt token-stream call-stack line)
  (define curr-token (first token-stream))
  (define second-token (first (rest token-stream)))
  (define third-token (first(rest (rest token-stream))))
  (define top-of-stack (first (call-stack)))
  (define popped-stack (rest call-stack))
  (cond
    [(string=? curr-token "id")(parse-token (match (match token-stream "id") "assign")(append `("expr") popped-stack ) line)]
    [(string=? curr-token "read") (parse-token (match (match token-stream "read") "id") popped-stack line)]
    [(string=? curr-token "write") (parse-token (match token-stream "write") (append `("expr") popped-stack ) line)]
    [else (parse-token (rest token-stream) `("ERROR") line)]
    ))

(define (expr token-stream call-stack line)
  (define curr-token (first token-stream))
  (define top-of-stack (first (call-stack)))
  (define popped-stack (rest call-stack))
  (cond
    [(string=? curr-token "lParen") (parse-token( token-stream (append `("term","term_tail") popped-stack) line)) ]
    [(string=? curr-token "id") (parse-token( token-stream (append `("term","term_tail") popped-stack) line))]
    [(string=? curr-token "num") (parse-token( token-stream (append `("term","term_tail") popped-stack) line))]    
    [else (parse-token (rest token-stream) `("ERROR") line)]
    ))

(define (term_tail token-stream call-stack line)
  (define curr-token (first token-stream))
  (define second-token (first (rest token-stream)))
  (define top-of-stack (first (call-stack)))
  (define popped-stack (rest call-stack))
  (cond
    [(and (string=? curr-token "add")) (parse-token( token-stream (append `("add_op","term","term_tail") popped-stack) line))]
    [(and (string=? curr-token "minus")) (parse-token( token-stream (append `("add_op","term","term_tail") popped-stack) line))]
    [(string=? curr-token "write") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "read") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "$$") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "id") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "rParen") (parse-token token-stream popped-stack line)]
    [else (parse-token (rest token-stream) `("ERROR") line)]
    ))

(define (term token-stream call-stack line)
  (define curr-token (first token-stream))
  (define second-token (first (rest token-stream)))
  (define top-of-stack (first (call-stack)))
  (define popped-stack (rest call-stack))
  (cond
    [(and (string=? curr-token "id")) (parse-token( token-stream (append `("factor","factor_tail") popped-stack) line))]
    [(and (string=? curr-token "num")) (parse-token( token-stream (append `("factor","factor_tail") popped-stack) line))]
    [(string=? curr-token "lParen") (parse-token( token-stream (append `("factor","factor_tail") popped-stack) line))] 
    [else (parse-token (rest token-stream) `("ERROR") line)]
    ))

(define (factor_tail token-stream call-stack line)
  (define curr-token (first token-stream))
  (define second-token (first (rest token-stream)))
  (define top-of-stack (first (call-stack)))
  (define popped-stack (rest call-stack))
  (cond
    [(and (string=? curr-token "multiply")) (parse-token( token-stream (append `("mult_op","factor","factor_tail") popped-stack) line))]
    [(and (string=? curr-token "divide")) (parse-token( token-stream (append `("mult_op","factor","factor_tail") popped-stack) line))]
    [(string=? curr-token "plus") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "minus") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "$$") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "id") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "rParen") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "read") (parse-token token-stream popped-stack line)]
    [(string=? curr-token "write") (parse-token token-stream popped-stack line)]
    [else (parse-token (rest token-stream) `("ERROR") line)]
    ))


  
(define (match token-stream token)
  (cond
    [(string=? first(token-stream) token) (rest token-stream)]
    [else `("ERROR")]
    ))