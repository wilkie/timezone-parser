;
; d-builder.scm
;
; helper code for D mixin generation
;

(define add-between
  (lambda (lst v)
    (if (<= (length lst) 1)
      lst
      (append
        (list 
          (first lst)
          v)
        (add-between (cdr lst) v)))))

(define pad-value
  (lambda (x padding)
    (if (number? x)
      (string-append
        (make-string (- padding (string-length (number->string x))) #\space)
        (number->string x))
      (string-append
        (make-string (- padding (string-length x)) #\space)
        "\""
        x
        "\""))))

(define comma-ize
  (lambda (x padding)
    (string-append
      (pad-value x padding)
      ", ")))

(define build-part
  (lambda (l padding)
    (string-append
      (apply
        string-append
        (map (lambda (x) (comma-ize x padding)) l)))))

(define build-line
  (lambda (l depth padding)
    (cond
      ((= (length l) 0)
       "")
      ((< (length l) 10)
       (build-part l padding))
      (else
        (string-append
          (build-part (take l 10) padding)
          "\n"
          (make-string depth #\tab)
          (build-line (list-tail l 10) depth padding))))))

(define maximum
  (lambda (lst)
    (if (= (length lst) 1)
      (car lst)
      (let ((a (car lst)) (b (maximum (cdr lst))))
        (if (> a b)
          a
          b)))))

(define get-max-length
  (lambda (l)
    (maximum
      (map 
        (lambda (x)
          (if (list? x)
            0
            (if (number? x)
              (string-length (number->string x))
              (+ (string-length x) 2))))
        l))))

; builds a list into an array literal
(define generate-d-array-literal-impl
  (lambda (l depth)
    (let ((padding (+ (get-max-length l) 1)))
      (cond
        ((= (length l) 0)
         (string-append
           "["
           "\n"
           (make-string (- depth 1) #\tab)
           "]"))
        (else
          (string-append
            "["
            (make-string depth #\tab)
            (cond
              ((list? (list-ref l 0))
               (string-append
                 (apply
                   string-append
                   (map
                     (lambda (x)
                       (string-append
                         "\n"
                         (make-string depth #\tab)
                         (generate-d-array-literal-impl x (+ depth 1))
                         ","))
                     (drop-right l 1)))
                 "\n"
                 (make-string depth #\tab)
                 (generate-d-array-literal-impl (last l) (+ depth 1))))
              (else
                (string-append
                  "\n"
                  (make-string depth #\tab)
                  (build-line (drop-right l 1) depth padding)
                  (pad-value (last l) padding))))
            "\n"
            (make-string (- depth 1) #\tab)
            "]"))))))

; builds a list into an array literal
(define generate-d-array-literal
  (lambda (lst)
    (generate-d-array-literal-impl lst 1)))

; print D code for the array
(define generate-d-array
  (lambda (lst name type)
    (cond
      ((= (length lst) 0)
       (string-append
         "const "
         type
         " "
         name
         " = "
         "[]"
         ";"))
      (else
        (string-append
          "const "
          type
          " "
          name
          " = "
          (generate-d-array-literal lst)
          ";")))))

; print D declaration
(define d-declaration
  (lambda (name type value)
    (list 
      (string-append
        type
        " "
        name
        " = "
        (d-value value)
        ";"
        ))))

(define d-value
  (lambda (val)
    (if (number? val)
      (number->string val)
      (string-append
        "\""
        val
        "\""))))

(define d-body-trim
  (lambda (lst)
    (cond 
      ((<= (length lst) 1)
       lst)
      ((string=? (first lst) "")
       (d-body-trim (cdr lst)))
      ((string=? (last lst) "")
       (d-body-trim (drop lst 1)))
      (else lst))))

(define d-body 
  (lambda (xtra)
    (d-body-trim
      (fold-left
        (lambda (lst e)
          (if (list? e)
            (if (and (string=? (first lst) "") (string=? (last e) ""))
              (append (cdr lst) e)
              (append lst e))
            (append lst (list e))))
        '("") 
        xtra))))

(define d-if
  (lambda (condition . xtra)
    (let ((body (d-body xtra)))
      (append
        (list "" (string-append "if (" condition ") {"))
        (map (lambda (x) (string-append "\t" x)) body)
        (list "}")))))

(define d-indent
  (lambda (amt str)
    (let ((newstrs (string-split str '(#\newline))))
      (cons
        (car newstrs)
        (map
          (lambda (str)
            (string-append (make-string amt #\space) str))
          (cdr newstrs))))))

(define d-else-if
  (lambda (condition . xtra)
    (let ((body (d-body xtra)))
      (append
        (d-indent 9 (string-append "else if (" condition ") {"))
        (map (lambda (x) (string-append "\t" x)) body)
        (list "}")))))

(define d-else
  (lambda xtra
    (let ((body (d-body xtra)))
      (append
        '("else {")
        (map (lambda (x) (string-append "\t" x)) body)
        (list "}" "")))))

(define d-comment
  (lambda xtra
    (let ((body (d-body xtra)))
      (map (lambda (x) (string-append "// " x)) body))))

(define d-comment-block
  (lambda xtra
    (let ((body (d-body xtra)))
      (append
        (cons
          (string-append "/* " (car body))
          (map (lambda (x) (string-append "   " x)) (drop-right (cdr body) 1)))
        (list (string-append "   " (last body) " */"))))))

; print D function
(define d-function
  (lambda (name type args . xtra)
    (let ((body (d-body xtra)))
      (append
        (list
          (string-append
            type
            " "
            name
            "("
            args
            ") {"))
        (map (lambda (x) (string-append "\t" x)) body)
        (list "}")))))

(define d-public
  (lambda ()
    '("public:")))

(define d-private
  (lambda ()
    '("private:")))

(define d-static
  (lambda ()
    '("static:")))

(define d-module
  (lambda (module-name)
    (list
      (string-append
        "module "
        module-name
        ";"))))

(define d-import
  (lambda (module-name)
    (list
      (string-append
        "import "
        module-name
        ";"))))

(define d-class
  (lambda (name . xtra)
    (let ((body (d-body xtra)))
      (append
        (list
          (string-append
            "class "
            name
            " {"))
        (map 
          (lambda (x) 
            (if (or (or (string=? x "public:") (string=? x "static:")) (string=? x "private:"))
              x
              (string-append "\t" x)))
          body)
        (list "}")))))

(define d-call
  (lambda (function-name . args)
    (list
      (string-append 
        function-name 
        "(" 
        (fold-left string-append "" (add-between (map d-value args) ", ")) 
        ");"))))

(define d-generate
  (lambda (lst)
    (if (> (length lst) 0 )
      (begin
        (display (car lst))
        (newline)
        (d-generate (cdr lst))))))

(define d-return
  (lambda (value)
    (list
      ""
      (string-append
        "return "
        (d-value value)
        ";"))))

(define d-safe-name
  (lambda (name)
    (string-replace name #\- #\_)))
