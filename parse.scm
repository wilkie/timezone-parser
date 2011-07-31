(load "parse-rules.scm")
(load "parse-zones.scm")

(load "scheme-builder.scm")
(load "d-builder.scm")

(define generate-rule
  (lambda (name)
    (d-generate
      (let ((all-rules (rule-pairs name)))
        (fold-left 
          (lambda (acc rule-pair)
            (let ((until-year (rule-pair-valid-until-year rule-pair)))
              (append
                acc
                (if (string? until-year)
                  (d-else)
                  (d-else-if (string-append "year <= " (number->string until-year)))))))
          (d-declaration "ret" "int" 0)
          all-rules)))))

(with-output-to-file "scheme-tzdata.scm"
  (lambda ()
    (d-generate
      (d-class "TimeZone"
        (d-static)
        (d-public)
        (d-function "foo" "uint" "int a"
          (d-if "foo < bah"
            (d-comment-block
              "hello"
              "world"
              "asdfasdf asfs"
              "asfsdf")
            (d-declaration "x" "uint" 2)
            (d-if "x < bah"
              (d-declaration "y" "uint" -42))
            (d-comment-block
              (d-if "x < bah"
                (d-declaration "y" "uint" -42))))
          (d-else
            (d-declaration "x" "uint" 3))
          (d-if "foo < bah"
            (d-declaration "x" "uint" 2))
          (d-else-if "foo < bah"
            (d-declaration "x" "uint" 2))
          (d-else
            (d-declaration "x" "uint" 3))
          (d-call "foo" 5 15 "hello")
          (d-declaration "ger" "uint" 7))))
    (display (rule-seconds (first (first (rule-pairs "SL")))))
    (newline)
    (display (rule-when-hour (first (first (rule-pairs "SL")))))
    (newline)
    (display (rule-when-minute (first (first (rule-pairs "SL")))))
    (newline)
    (display (rule-when-second (first (first (rule-pairs "SL")))))
    (newline)
    (generate-scheme-define "Rules" (rule-pairs "Namibia") "")
    (newline)
    (newline)))
