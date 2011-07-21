(load "parse-rules.scm")
(load "parse-zones.scm")

(load "scheme-builder.scm")
(load "d-builder.scm")

(with-output-to-file "scheme-tzdata.scm"
  (lambda ()
    (display (rule-pairs "Algeria"))
    (newline)
    (display (rule-pairs "Egypt"))
    (newline)
    (display (rule-pairs "Namibia"))
    (newline)
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
    (newline)))
