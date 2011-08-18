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
          (d-module (string-append "tzcode.rules." category)))
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

(define day-code
  (lambda (day month year)
    (if (number? (string->number day))
      day
      (if (>= (string-length day) 5)
        (let ((day-of-week (substring day 0 3))
              (day-of-month (substring day 5 (string-length day))))
          (string-append
            (if (string=? day-of-week "las")
              (string-append
                "Util.isLast("
                (day-of-week-number (substring day 4 (string-length day))))
              (string-append
                "Util.isFirst("
                (day-of-week-number day-of-week)
                ", "
                day-of-month))
            ", "
            (string year)
            ", "
            (string month)
            ")"))))))

(define generate-zone-offset-body
  (lambda (zone-law is-first)
    (let ((d-func (if is-first d-if d-else-if)))
      (d-func
        (string-append
          "Util.isStrictlyBefore(year, month, day, hour, minute, "
          (string (zone-law-year zone-law))
          ", "
          (string (zone-law-month zone-law))
          ", "
          (string (day-code (zone-law-day zone-law) (zone-law-month zone-law) (zone-law-year zone-law)))
          ","
          (string (zone-law-when-hour zone-law))
          ","
          (string (zone-law-when-minute zone-law))
          ")")
        (d-return (zone-law-offset-seconds zone-law))))))

(define generate-zone-savings-body
  (lambda (zone-law is-first)
    (let ((d-func (if is-first d-if d-else-if))
          (rule (zone-law-rule zone-law)))
      (d-func
        (string-append
          "Util.isStrictlyBefore(year, month, day, hour, minute, "
          (string (zone-law-year zone-law))
          ", "
          (string (zone-law-month zone-law))
          ", "
          (string (day-code (zone-law-day zone-law) (zone-law-month zone-law) (zone-law-year zone-law)))
          ","
          (string (zone-law-when-hour zone-law))
          ","
          (string (zone-law-when-minute zone-law))
          ")")
        (d-return 
          (if (null? rule)
            0
            (d-call 
              (string-append
                (d-safe-name (string-titlecase (rule-category rule)))
                "Rules"
                "."
                (d-safe-name (string-titlecase (rule-name rule))) 
                "Rule.savings")
              (d-var "year")
              (d-var "month")
              (d-var "day")
              (d-var "hour")
              (d-var "minute"))))))))

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
              (string (day-code (rule-day (car pair)) (rule-month (car pair)) (rule-start (car pair))))
              ", "
              (string (rule-when-hour (car pair)))
              ", "
              (string (rule-when-minute (car pair)))
              ") &&\nUtil.isBefore(year, month, day, hour, minute, "
              (string (rule-end (cadr pair)))
              ", "
              (string (rule-month (cadr pair)))
              ", "
              (string (day-code (rule-day (car pair)) (rule-month (car pair)) (rule-end (car pair))))
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
          (module-name (d-modulize (zone-name zone))))
      (with-output-to-file (string-append "tzcode/zones/" module-name ".d")
        (lambda ()
          (d-generate
            (d-module (string-append "tzcode.zones." module-name)))
          (newline)
          (d-generate
            (d-import "tzcode.util"))
          (newline)
          (map
            (lambda (category)
              (d-generate
                (d-import (string-append "tzcode.rules." category))))
            (rule-categories))
          (newline)
          (d-generate
            (d-class (string-append (d-classify name) "Zone")
              (d-static)
              (d-public)
              (d-function "savings" "long" "long year, uint month, uint day, uint hour, uint minute"
                (d-body
                  (cons
                    (generate-zone-savings-body (car (zone-laws zone)) #t)
                    (map (lambda (zone-law) (generate-zone-savings-body zone-law #f)) (cdr (zone-laws zone)))))
                (d-return 0))
              (d-function "offset" "long" "long year, uint month, uint day, uint hour, uint minute"
                (d-body
                  (cons
                    (generate-zone-offset-body (car (zone-laws zone)) #t)
                    (map (lambda (zone-law) (generate-zone-offset-body zone-law #f)) (cdr (zone-laws zone)))))
                (d-return 0))))
          (newline))))))

(define sorted-zones-impl
  (lambda ()
    (sort 
      (zones)
      (lambda (zone-a zone-b)
        (string<? (zone-name zone-a) (zone-name zone-b))))))

(define sorted-zones
  (let ((zones-list '()))
    (lambda ()
      (if (null? zones-list)
        (let ((temp (sorted-zones-impl)))
          (set! zones-list temp)
          zones-list)
        zones-list))))

(define foo-bar '(("aardvark") ("horse") ("zebra")))

(define build-zone-tree-impl
  (lambda (lst)
    (let ((mid (integer-floor (length lst) 2)))
      (let ((left-hand (take lst mid))
            (right-hand (take-right lst (- (length lst) mid))))
        (d-body
          (list
            (d-if (d-op== "timezone" (d-value (zone-name (car right-hand))))
              (d-op= "offset" (d-addressof (string-append "Zones." (d-classify (zone-name (car right-hand))) ".offset")))
              (d-op= "savings" (d-addressof (string-append "Zones." (d-classify (zone-name (car right-hand))) ".savings")))
              (d-return #t))
            (if (null? left-hand)
              '("")
              (d-else-if (d-op< "timezone" (d-value (zone-name (car right-hand))))
                (build-zone-tree-impl left-hand)))
            (if (<= (length right-hand) 1)
              '("")
              (d-else
                (build-zone-tree-impl (cdr right-hand))))))))))

(define build-zone-tree
  (lambda ()
    (build-zone-tree-impl (sorted-zones))))

(define generate-time-zone-file
  (lambda ()
    (with-output-to-file "tzcode/time_zone.d"
      (lambda ()
        (d-generate
          (d-module "tzcode.time_zone"))
        (newline)
        (d-generate
          (d-body
            (map
              (lambda (zone)
                (d-import (string-append "tzcode.zones." (d-modulize (zone-name zone)))))
              (sorted-zones))))
        (newline)
        (d-generate
          (d-class "TimeZone"
            (d-static)
            (d-public)
            ; binary search to find implementations of timezone routines
            (d-function "funcs" "bool" (string-append
                                         "char[] timezone,"
                                         "\n\t           ref long delegate(long, uint, uint, uint, uint) offset,"
                                         "\n\t           ref long delegate(long, uint, uint, uint, uint) savings")
              (build-zone-tree)
              (d-return #f))))
        (newline)))))

(map
  generate-rule-file
  (rule-categories))

(map 
  generate-zone-file
  (zones))

(generate-time-zone-file)

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
