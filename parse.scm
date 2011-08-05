(load "parse-rules.scm")
(load "parse-zones.scm")

(load "scheme-builder.scm")
(load "d-builder.scm")

(define string-titlecase
  (lambda (str)
    (if (= (string-length str) 0)
      str
      (string-append
        (string-upcase (substring str 0 1))
        (string-downcase (substring str 1 (string-length str)))))))

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

(define generate-rule-file
  (lambda (category)
    (with-output-to-file (string-append "tzcode/rules/" category ".d")
      (lambda ()
        (d-generate
          (d-module (string-append "tzcode.rules" category)))
        (newline)
        (d-generate
          (d-import "tzcode.util"))
        (newline)
        (d-generate
          (d-class (string-append (string-titlecase category) "Rules")
            (d-static)
            (d-public)
            (d-body
              (map
                generate-rule-class
                (rule-names category)))))
        (newline)))))

(define day-of-week-number
  (lambda (str)
    (cond ((string=? str "Sun") "0")
          ((string=? str "Mon") "1")
          ((string=? str "Tue") "2")
          ((string=? str "Wed") "3")
          ((string=? str "Thu") "4")
          ((string=? str "Fri") "5")
          ((string=? str "Sat") "6")
          (else #f))))

(define rule-day-code
  (lambda (rule is-start)
    (let ((rule-day-actual (rule-day rule)))
      (if (number? (string->number rule-day-actual))
        rule-day-actual
        (if (>= (string-length rule-day-actual) 5)
          (let ((day-of-week (substring rule-day-actual 0 3))
                (day-of-month (substring rule-day-actual 5 (string-length rule-day-actual))))
            (string-append
              (if (string=? day-of-week "las")
                (string-append
                  "Util.isLast("
                  (day-of-week-number (substring rule-day-actual 4 (string-length rule-day-actual))))
                (string-append
                  "Util.isFirst("
                  (day-of-week-number day-of-week)
                  ", "
                  day-of-month))
              ", "
              (if is-start
                (string (rule-start rule))
                (string (rule-end rule)))
              ", "
              (string (rule-month rule))
              ")")))))))

(define generate-rule-body
  (lambda (rule-pair)
    (d-body 
      (map
        (lambda (pair)
          (d-else-if 
            (string-append
              "Util.isAfter(year, month, day, hour, minute, "
              (string (rule-start (car pair)))
              ", "
              (string (rule-month (car pair)))
              ", "
              (string (rule-day-code (car pair) #t))
              ", "
              (string (rule-when-hour (car pair)))
              ", "
              (string (rule-when-minute (car pair)))
              ") &&\nUtil.isBefore(year, month, day, hour, minute, "
              (string (rule-end (cadr pair)))
              ", "
              (string (rule-month (cadr pair)))
              ", "
              (string (rule-day-code (cadr pair) #f))
              ", "
              (string (rule-when-hour (cadr pair)))
              ", "
              (string (rule-when-minute (cadr pair)))
              ")")
            (d-return (rule-seconds (car pair)))))
        (map
          (lambda (pair)
            (list
              (car rule-pair)
              pair))
          (cadr rule-pair))))))

(define generate-rule-class
  (lambda (rule-name)
    (let ((pairs (rule-pairs rule-name)))
      (cons
        ""
        (d-class (string-append (d-safe-name (string-titlecase rule-name)) "Rule")
          (d-static)
          (d-public)
          (d-function "savings" "long" "long year, uint month, uint day, uint hour, uint minute"
            (d-if (string-append "year < " (string (rule-pairs-first-year pairs)))
              (d-return 0))
            (d-body
              (map generate-rule-body pairs))
            (d-return 0)))))))

(define generate-zone-file
  (lambda (zone)
    (let ((name (zone-name zone))
          (module-name (d-classify (zone-name zone))))
      (with-output-to-file (string-append "tzcode/zones/" module-name ".d")
        (lambda ()
          (d-generate
            (d-module (string-append "tzcode.zones." module-name)))
          (newline)
          (d-generate
            (d-import "tzcode.util"))
          (newline)
;          (d-generate
;            (d-class (string-append (string-titlecase category) "Rules")
;              (d-static)
;              (d-public)
;              (d-body
;                (map
;                  generate-rule-class
;                  (rule-names category)))))
          (newline))))))


(map
  generate-rule-file
  (rule-categories))

(map 
  generate-zone-file
  (zones))

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
    (generate-scheme-define "Zones" (zones) "")
    (display (zone-law-offset (car (zone-laws (car (zones))))))
    (newline)
    (display (zone-law-offset-seconds (car (zone-laws (car (zones))))))
    (newline)
    (display (zone-law-offset (car (zone-laws (car (cddddr (zones)))))))
    (newline)
    (display (zone-law-offset-seconds (car (zone-laws (car (cddddr (zones)))))))
    (newline)))
