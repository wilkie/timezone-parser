(load "parse-util.scm")

(define in_rules 
  (open-input-file "rules"))

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
    (let ((rule-mth (rule-month-actual rule)))
      (cond ((string=? rule-mth "Jan")  1)
            ((string=? rule-mth "Feb")  2)
            ((string=? rule-mth "Mar")  3)
            ((string=? rule-mth "Apr")  4)
            ((string=? rule-mth "May")  5)
            ((string=? rule-mth "Jun")  6)
            ((string=? rule-mth "Jul")  7)
            ((string=? rule-mth "Aug")  8)
            ((string=? rule-mth "Sep")  9)
            ((string=? rule-mth "Oct")  10)
            ((string=? rule-mth "Nov")  11)
            ((string=? rule-mth "Dec")  12)
            (else 0)))))

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
    (string->number (second (string-split (rule-when rule) '(#\:))))))

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
    (filter
      (lambda (rule-to-check)
        (terminating-rule? rule rule-to-check))
      all-rules)))

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
