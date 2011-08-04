(load "parse-util.scm")

(define in_zones
  (open-input-file "zones"))

(define zones-parse
  (lambda ()
    (let
      ((next-zone (input-zone)))
      (if (eof-object? next-zone)
        ()
        (cons
          next-zone
          (zones-parse))))))

(define zones
  (let ((zones-list '()))
    (lambda ()
      (if (null? zones-list)
        (let ((temp (zones-parse)))
          (set! zones-list temp)
          zones-list)
        zones-list))))

(define input-zone-entries
  (lambda ()
    (let
      ((line (read-line in_zones)))
      (if (eof-object? line)
        ()
        (let
          ((tokens (line-until-comment (cdr (string-split line)))))
          (if (= (length tokens) 3)
            (list tokens)
            (cons tokens (input-zone-entries))))))))

(define input-zone
  (lambda ()
    (let
      ((line (read-line in_zones)))
      (if (eof-object? line)
        line
        (let
          ((tokens (line-until-comment (string-split line))))
          (let
            ((name (cadr tokens)) (entries (cddr tokens)))
            (list name (cons entries (input-zone-entries)))))))))

(define zone-name
  (lambda (zone)
    (car zone)))

(define zone-laws
  (lambda (zone)
    (cadr zone)))

(define zone-law-offset
  (lambda (zone-law)
    (car zone-law)))

(define zone-law-offset-negative?
  (lambda (zone-law)
    (string=? (substring (zone-law-offset zone-law) 0 1) "-")))

(define zone-law-offset-abs
  (lambda (zone-law)
    (if (zone-law-offset-negative? zone-law)
      (substring (zone-law-offset zone-law) 1 (string-length (zone-law-offset zone-law)))
      (zone-law-offset zone-law))))

(define zone-law-offset-seconds
  (lambda (zone-law)
    (*
      (if (zone-law-offset-negative? zone-law)
        -1
        1)
      (*
        (if (= (length (string-split (zone-law-offset-abs zone-law) '(#\:))) 2)
          60
          1)
        (fold-left
          (lambda (acc str)
            (+
              (* acc 60)
              (string->number str)))
          0
          (string-split (zone-law-offset-abs zone-law) '(#\:)))))))

(define zone-law-rule-name
  (lambda (zone-law)
    (cadr zone-law)))

(define zone-law-human-string
  (lambda (zone-law)
    (caddr zone-law)))

(define zone-law-year
  (lambda (zone-law)
    (if (>= (length zone-law) 4)
      (cadddr zone-law)
      3200000)))

(define zone-law-month-actual
  (lambda (zone-law)
    (if (>= (length zone-law) 5)
      (car (cddddr zone-law))
      "Jan")))

(define zone-law-month
  (lambda (zone-law)
    (month-string-to-number (zone-law-month-actual zone-law))))

(define zone-law-day
  (lambda (zone-law)
    (if (>= (length zone-law) 6)
      (cadr (cddddr zone-law))
      1)))

(define zone-law-when
  (lambda (zone-law)
    (if (>= (length zone-law) 7)
      (caddr (cddddr zone-law))
      "0:00")))

(define zone-law-when-hour
  (lambda (zone-law)
    (string->number (first (string-split (zone-law-when zone-law) '(#\:))))))

(define zone-law-when-minute
  (lambda (zone-law)
    (let ((split (string-split (zone-law-when zone-law) '(#\:))))
      (if (= (length split) 1)
        0
        (string->number (substring (second split) 0 2))))))

(define zone-law-when-second
  (lambda (zone-law)
    (let ((split (string-split (zone-law-when zone-law) '(#\:))))
      (if (= (length split) 3)
        (string->number (third (split)))
        0))))
