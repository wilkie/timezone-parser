(load "parse-util.scm")

(define in_rules 
  (open-input-file "tzdata/rules"))

(define rules-parse
  (lambda ()
    (let
      ((next-rule (input-rule)))
      (if (eof-object? next-rule)
        ()
        (cons
          next-rule
          (rules-parse))))))

(define rules
  (let ((rules-list '()))
    (lambda ()
      (if (null? rules-list)
        (let ((temp (rules-parse)))
          (set! rules-list temp)
          rules-list)
        rules-list))))

(define input-rule
  (lambda ()
    (let
      ((line (read-line in_rules)))
      (if (eof-object? line)
        line
        (line-until-comment
          (string-split
            line))))))

(define rule-category
  (lambda (rule)
    (substring (car rule) 0 (- (string-length (car rule)) 5))))

(define rule-name
  (lambda (rule)
    (cadr rule)))

(define rule-start-actual
  (lambda (rule)
    (caddr rule)))

(define rule-start
  (lambda (rule)
    (if (string=? (rule-start-actual rule) "min")
      -3200000
      (string->number (rule-start-actual rule)))))

(define rule-end-actual
  (lambda (rule)
    (cadddr rule)))

(define rule-end
  (lambda (rule)
    (if (string=? (rule-end-actual rule) "only")
      (rule-start rule)
      (if (string=? (rule-end-actual rule) "max")
        3200000
        (string->number (rule-end-actual rule))))))

(define rule-month-actual
  (lambda (rule)
    (cadddr (cddr rule))))

(define rule-month
  (lambda (rule)
    (month-string-to-number (rule-month-actual rule))))

(define rule-day
  (lambda (rule)
    (cadddr (cdddr rule))))

(define rule-when
  (lambda (rule)
    (cadddr (cddddr rule))))

(define rule-when-hour
  (lambda (rule)
    (string->number (first (string-split (rule-when rule) '(#\:))))))

(define rule-when-minute
  (lambda (rule)
    (let ((split (string-split (rule-when rule) '(#\:))))
      (if (= (length split) 1)
        0
        (string->number (substring (second split) 0 2))))))

(define rule-when-second
  (lambda (rule)
    (let ((split (string-split (rule-when rule) '(#\:))))
      (if (= (length split) 3)
        (string->number (third (split)))
        0))))

(define rule-save
  (lambda (rule)
    (cadddr (cddddr (cdr rule)))))

(define rule-char
  (lambda (rule)
    (cadddr (cddddr (cddr rule)))))

(define find-rules
  (lambda (name)
    (filter 
      (lambda (item)
        (string=? (rule-name item) name))
      (rules))))

(define rule-seconds
  (lambda (rule)
    (*
      60
      (fold-left
        (lambda (acc str)
          (+ 
            (* acc 60)
            (string->number str)))
        0
        (string-split (rule-save rule) '(#\:))))))

(define month-less-than?
  (lambda (rule-a rule-b)
    (< (rule-month rule-a) (rule-month rule-b))))

(define terminating-rule?
  (lambda (rule-savings rule-to-check)
    (let ((year-start-savings (rule-start rule-savings))
          (year-end-savings (rule-end rule-savings))
          (year-start-check (rule-start rule-to-check))
          (year-end-check (rule-end rule-to-check)))
      (and 
        (= (rule-seconds rule-to-check) 0)
        (or 
          (and ; start-check within range
            (>= year-start-savings year-start-check)
            (<= year-start-savings year-end-check))
          (and ; end-check within range
            (>= year-end-savings year-start-check)
            (<= year-end-savings year-end-check)))))))

(define find-rule-savings
  (lambda (all-rules rule)
    (let ((terminating-rules (filter
                               (lambda (rule-to-check)
                                 (terminating-rule? rule rule-to-check))
                               all-rules)))
      (if (null? terminating-rules)
        (filter
          (lambda (rule-to-check)
            (and
              (= (rule-seconds rule-to-check) 0)
              (= (rule-end rule) (- (rule-start rule-to-check) 1))))
          all-rules)
        terminating-rules))))

(define rule-pairs
  (lambda (name)
    (let ((all-rules (find-rules name)))
      (map
        (lambda (rule)
          (list
            rule
            (find-rule-savings all-rules rule)))
        (filter
          (lambda (rule)
            (not (= (rule-seconds rule) 0)))
          all-rules)))))

(define rule-pair-valid-until-year
  (lambda (rule-pair)
    (let ((year-a (rule-end (first rule-pair)))
          (year-b (rule-end (second rule-pair))))
      (if (string=? year-a "max")
        year-b
        (if (string=? year-b "max")
          year-a
          (min (string->number year-a) (string->number year-b)))))))

(define find-rule-categories
  (lambda ()
    (map
      (lambda (category)
        (substring category 0 (- (string-length category) 5)))
      (remove-duplicates
        (map
          car
          (rules))))))

(define rule-names
  (lambda (category)
    (remove-duplicates
      (map
        rule-name
        (filter
          (lambda (rule)
            (string=? (string-append category ":Rule") (car rule)))
          (rules))))))

(define rule-categories
  (let ((names-list '()))
    (lambda ()
      (if (null? names-list)
        (let ((temp (find-rule-categories)))
          (set! names-list temp)
          names-list)
        names-list))))

(define rule-pairs-first-year-impl
  (lambda (pairs year)
    (if (null? pairs)
      year
      (rule-pairs-first-year-impl (cdr pairs) (min year (rule-start (caar pairs)))))))

(define rule-pairs-first-year
  (lambda (pairs)
    (rule-pairs-first-year-impl pairs 3200000)))
