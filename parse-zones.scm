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


