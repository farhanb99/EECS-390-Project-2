; Lexer for Scheme. The following is the lexical specification that it
; handles:
;
; <token> --> <identifier> | <boolean> | <number>
;     | <character> | <string> | ( | ) | #( | ' | ` | , | ,@ | .
; <delimiter> --> <whitespace> | ( | ) | " | ;
; <whitespace> --> <space or newline>
; <comment> --> ; <all subsequent characters up to a line break>
; <atmosphere> --> <whitespace> | <comment>
; <intertoken space> --> <atmosphere>*
;
; <identifier> --> <initial> <subsequent>*
;     | <peculiar identifier>
; <initial> --> <letter> | <special initial>
; <letter> --> [a-z]
;
; <special initial> --> ! | $ | % | & | * | / | : | < | =
;     | > | ? | ^ | _ | ~
; <subsequent> --> <initial> | <digit> | <special subsequent>
; <digit> --> [0-9]
; <special subsequent> --> + | - | . | @
; <peculiar identifier> --> + | - | ...
;
; <boolean> --> #t | #f
; <character> --> #\ <any character> | #\ <character name>
; <character name> --> space | newline
;
; <string> --> " <string element>* "
; <string element> --> <any character other than " or \>
;     | \" | \\
;
; <number> --> <integer> | <decimal>
; <integer> --> <sign> <digit>+
; <decimal> --> <sign> <digit>+ . <digit>*
;     | <sign> . <digit>+
;
; <sign> --> <empty> | + | -
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "distribution.scm")

;;;;;;;;;;;;;;;;;;;

; Read a string token.
(define (read-string)
  (if (read-start #\" "not a string") ; string must start with "
      (read-string-tail '()) ; call helper function below
  )
)

; Read the rest of a string literal.
(define (read-string-tail read-so-far)
  (let ((next-char (get-non-eof-char))) ; read a single char
    (cond ((char=? next-char #\") ; end of string
           ; return a string token
           (token-make 'string (list->string (reverse read-so-far))))
          ((char=? next-char #\\) ; start of escape sequence
           ; read the rest of the escape sequence and recurse
           (read-string-tail (cons (read-escaped) read-so-far)))
          ; complete this procedure
          (else (read-string-tail (append (list next-char) read-so-far)))
    )
  )
)

; Read the rest of an escape sequence.
(define (read-escaped)
  (let ((escaped-char (get-non-eof-char)))
    (if (or (char=? escaped-char #\") (char=? escaped-char #\\))
        escaped-char
        (error "unrecognized escape sequence")
    )
  )
)

;;;;;;;;;;;;;;;;;;;

; Read a boolean token.
(define (read-boolean)
  (if (read-start #\# "not a boolean") ; boolean starts with #
      (read-boolean-tail)
  )
)

; Read the rest of a boolean literal.
(define (read-boolean-tail)
  (let ((next-char (get-non-eof-char)))
    (cond ((char=? next-char #\t) ; true
           ; return a string token
           (token-make 'boolean #t))
          ((char=? next-char #\f) ; false
           (token-make 'boolean #f))
          ; raise error
          (else (error "not a boolean"))
    )
  )
)

;;;;;;;;;;;;;;;;;;;

; Read a character token.
(define (read-character)
  (if (and (read-start #\# "not a character")  ; character must start
           (read-start #\\ "not a character")) ; with #\
      (read-character-tail)
  )
)

; Read the rest of a character literal.
(define (read-character-tail)
  (let ((char (read-char)))
    (if (delimiter? (peek-char))
        (token-make 'character char)
        (cond ((char=? char #\s)
           (if (token-match (string->list "pace"))
               (token-make 'character #\space)
               (error "not a character token")
           ))
          ((char=? char #\n)
           (if (token-match (string->list "ewline"))
               (token-make 'character #\newline)
               (error "not a character token")
           ))
          (else (error "character doesn't end in delimiter"))
        )
    )
  )
)

; Determine if a token matches expected
(define (token-match expected)
  (if (equal? (length expected) 0)
      (if (delimiter? (peek-char))
          #t
          (error "token doesn't end in delimiter")
      )
      (if (char=? (read-char) (car expected))
          (token-match (cdr expected))
          #f
      )
  )
)

;;;;;;;;;;;;;;;;;;;

; Determine if the given character is a sign character.
(define (sign? char)
  (or (char=? char #\+) (char=? char #\-))
)

; Determine if the given character is a digit.
(define (digit? char)
  (and (char>=? char #\0) (char<=? char #\9))
)

; Read a number token.
(define (read-number)
  (let ((char (read-char)))
    (cond ((sign? char)
           (read-number-tail (list char)))
          ((digit? char)
           (read-number-tail (list char)))
          ((char=? char #\.)
           (read-decimal (list char)))
          (else (error "not a number"))
    )
  )
)

; Read the rest of a number token.
(define (read-number-tail read-so-far)
  (if (delimiter? (peek-char))
      (token-make 'number (string->number (list->string read-so-far)))
      (let ((char (read-char)))
        (cond ((digit? char)
               (read-number-tail (append read-so-far (list char))))
              ((char=? char #\.)
               (read-decimal (append read-so-far (list char))))
              (else (error "not a number"))
        )
      )
  )
)

; Read the rest of a decimal number.
(define (read-decimal read-so-far)
  (if (delimiter? (peek-char))
      (token-make 'number (string->number (list->string read-so-far)))
      (let ((char (read-char)))
        (if (digit? char)
            (read-decimal (append read-so-far (list char)))
            (error "not a number")
        )
      )
  )
)

;;;;;;;;;;;;;;;;;;;


; Read an identifier token.
(define (read-identifier)
  (let ((char (char-downcase (read-char))))
    (cond ((sign? char)
           (if (delimiter? (peek-char))
               (token-make 'identifier (string->symbol (string char)))
               (error "not an identifier")
           ))
          ((char=? char #\.)
           (if (token-match (string->list ".."))
               (token-make 'identifier (string->symbol "..."))
               (error "not an identifier")
           ))
          ((initial? char)
           (read-identifier-tail (list char)))
          (else (error "not an identifier"))
    )
  )
)

; Read the rest of an identifier token.
(define (read-identifier-tail read-so-far)
  (if (delimiter? (peek-char))
      (token-make 'identifier (string->symbol (list->string read-so-far)))
      (let ((char (char-downcase (read-char))))
        (if (subsequent? char)
            (read-identifier-tail (append read-so-far (list char)))
            (error "not an identifier")
        )
      )
  )
)

; Check if a character is an initial character of an identifier.
(define (initial? char)
  (cond ((char-alphabetic? char) #t)
        ((special-initial? char) #t)
        (else #f)
  )
)

; Check if a character is a subsequent character of an identifier.
(define (subsequent? char)
  (cond ((digit? char) #t)
        ((char-alphabetic? char) #t)
        ((special-initial? char) #t)
        ((special-subsequent? char) #t)
        (else #f)
  )
)

; Check if character is a special initial character of an identifier.
(define (special-initial? char)
  (cond ((char=? #\! char) #t)
        ((char=? #\$ char) #t)
        ((char=? #\% char) #t)
        ((char=? #\& char) #t)
        ((char=? #\* char) #t)
        ((char=? #\/ char) #t)
        ((char=? #\: char) #t)
        ((char=? #\< char) #t)
        ((char=? #\= char) #t)
        ((char=? #\> char) #t)
        ((char=? #\? char) #t)
        ((char=? #\^ char) #t)
        ((char=? #\_ char) #t)
        ((char=? #\~ char) #t)
        (else #f)
  )
)

; Check if character is a special subsequent character of an identifier.
(define (special-subsequent? char)
  (cond ((sign? char) #t)
        ((char=? #\. char) #t)
        ((char=? #\@ char) #t)
        (else #f)
  )
)

;;;;;;;;;;;;;;;;;;;


; Read a punctuator token (i.e. one of ( ) #( . ' ` , ,@ ).
(define (read-punctuator)
  (let ((char (read-char)))
    (cond ((char=? char #\()
           (token-make 'punctuator (string char)))
          ((char=? char #\))
           (token-make 'punctuator (string char)))
          ((char=? char #\.)
           (token-make 'punctuator (string char)))
          ((char=? char #\')
           (token-make 'punctuator (string char)))
          ((char=? char #\`)
           (token-make 'punctuator (string char)))
          ((char=? char #\#)
           (if (char=? (read-char) #\()
               (token-make 'punctuator "#(")
               (error "not a punctuator")
           ))
          ((char=? char #\,)
           (if (char=? (peek-char) #\@)
               (begin (read-char)
                 (token-make 'punctuator ",@")
               )
               (token-make 'punctuator ",")
           ))
          (else (error "not a punctuator"))
    )
  )
)

;;;;;;;;;;;;;;;;;;;

; Read a comment. Discards the data and returns an unspecified value.
(define (read-comment)
  (if (read-start #\; "not a comment")
      (read-comment-tail)
  )
)

; Read the rest of a comment.
(define (read-comment-tail)
  (clear-line)
)


;;;;;;;;;;;;;;;;;;;

; Read a token, which can be a boolean, character, string, identifier,
; number, or punctuator. Discards whitespace and comments.
(define (read-token)
  (let ((next-char (peek-char)))
    (cond ((eof-object? next-char) ; eof
           (read-char)) ; just return eof
          ((whitespace? next-char) ; whitespace
           (read-char) ; discard it
           (read-token)) ; read another token
          ((char=? next-char #\;) ; comment
           (read-comment) ; discard it
           (read-token)) ; read another token
          ((char=? next-char #\") ; string
           (read-string))
          ((digit? next-char) ; number
           (read-number))
          ((sign? next-char) ; number
           (read-number))
          ((initial? next-char) ; identifier
           (read-identifier))
          ((char=? next-char #\.)
           (read-char)
           (read-dot))
          ((char=? next-char #\#)
           (read-char)
           (read-hashtag))
          (else
           (read-punctuator)) ; punctuator
    )
  )
)

; Lex the rest of a token that starts with a dot.
(define (read-dot)
  (let ((next-char (peek-char)))
    (cond ((digit? next-char) ; number
            (read-decimal (string->list ".")))
          ((char=? next-char #\.) ; identifier
            (if (token-match (string->list ".."))
              (token-make 'identifier (string->symbol "..."))
              (error "bad token")
            ))
          (else
            (token-make 'punctuator ".")) ; punctuator
    )
  )
)

; Lex the rest of a token that starts with a hashtag.
(define (read-hashtag)
  (let ((next-char (read-char)))
    (cond ((char=? next-char #\\) ; character
            (read-character-tail))
          ((char=? next-char #\() ; punctuator
            (token-make 'punctuator "#("))
          ((char=? next-char #\t) ; boolean
            (token-make 'boolean #t))
          ((char=? next-char #\f) ; boolean
            (token-make 'boolean #f))
          (else
            (error "bad token"))
    )
  )
)
