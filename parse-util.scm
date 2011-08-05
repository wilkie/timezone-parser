(define inc
  (lambda (i)
    (+ i 1)))

(define (string-split str . rest)
                ; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (inc i) yet-to-split-count))
        (else (scan-beg-word (inc i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

                ; maxsplit is a positive number
                ; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

                        ; resolver of overloading...
                        ; if omitted, maxsplit defaults to
                        ; (inc (string-length str))
  (if (string-null? str) '()
    (if (null? rest) 
      (split-by-whitespace str (inc (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (inc (string-length str)))))
        (cond
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)

(define line-until-comment
  (lambda (tokens)
    (if (= (length tokens) 0)
      ()
      (if (string-prefix? "#" (car tokens))
        ()
        (cons 
          (car tokens)
          (line-until-comment (cdr tokens)))))))

(define (remove-duplicates l)
  (do ((a '() (if (member (car l) a) a (cons (car l) a)))
       (l l (cdr l)))
    ((null? l) (reverse a))))

(define month-string-to-number
  (lambda (rule-mth)
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
          (else 0))))
