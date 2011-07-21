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

(define rule-start
  (lambda (rule)
    (caddr rule)))

(define rule-end-actual
  (lambda (rule)
    (cadddr rule)))

(define rule-end
  (lambda (rule)
    (if (string=? (rule-end-actual rule) "only")
      (rule-start rule)
      (rule-end-actual rule))))

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

(define rule-years
  (lambda (name)
    (remove-duplicates
      (map rule-start (find-rules name)))))

(define month-less-than?
  (lambda (rule-a rule-b)
    (< (rule-month rule-a) (rule-month rule-b))))

(define rule-pair-for-year
  (lambda (name year)
    (let ((all-rules (sort (find-rules name) month-less-than?)))
      (filter
        (lambda (item)
          (and 
            (or 
              (string=? (rule-start item) "min")
              (>= (string->number year) (string->number (rule-start item))))
            (or 
              (string=? (rule-end item) "max")
              (<= (string->number year) (string->number (rule-end item))))))
        all-rules))))

(define rule-pairs
  (lambda (name)
    (map
      (lambda (year)
        (rule-pair-for-year name year))
      (rule-years name))))
