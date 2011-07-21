(define generate-scheme-list-byline-impl
  (lambda (lst padding inline)
    (if (= (length lst) 0)
      ()
      (let ((item (car lst)))
        (if (list? item)
          (begin
            (if inline
              (begin
                (display "(")
                (generate-scheme-list-byline-impl item padding #t)
                (display ")")
                (if (= (length lst) 1)
                  ()
                  (begin
                    (newline)
                    (display padding))))
              (generate-scheme-list item (string-append "  " padding)))
            (generate-scheme-list-byline-impl (cdr lst) padding #t))
          (begin
            (write item)
            (if (= (length lst) 1)
              ()
              (display " "))
            (generate-scheme-list-byline-impl (cdr lst) padding #f)))))))

(define generate-scheme-list
  (lambda (lst padding)
    (begin
      (display "(")
      (newline)
      (display padding)
      (generate-scheme-list-byline-impl lst padding #t)
      (display ")"))))

(define generate-scheme-define
  (lambda (name lst padding)
    (display padding)
    (display "(define ")
    (display name)
    (display " ")
    (generate-scheme-list lst (string-append "  " padding))
    (display ")")
    (newline)))
