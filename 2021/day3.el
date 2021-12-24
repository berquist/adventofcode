;; -*- lexical-scope: t -*-

(require 'ert)
(require 'subr-x)
(require 'f)
(require 's)

(defconst example
  '("00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"))
(defconst example-bns
  '((0 0 1 0 0)
    (1 1 1 1 0)
    (1 0 1 1 0)
    (1 0 1 1 1)
    (1 0 1 0 1)
    (0 1 1 1 1)
    (0 0 1 1 1)
    (1 1 1 0 0)
    (1 0 0 0 0)
    (1 1 0 0 1)
    (0 0 0 1 0)
    (0 1 0 1 0)))
(defconst example-on-counts (vector 7 5 8 7 5))
(defconst example-ans 198)
(defconst example-ans-p2 230)

;; https://stackoverflow.com/a/20577329
(defun int-to-binary-string (i)
  "Convert an integer into its binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun binary-string-to-int (bs)
  (string-to-number bs 2))

(defun make-bns (lines)
  (mapcar
   (lambda (line)
     (mapcar #'string-to-number (mapcar #'char-to-string (string-to-list line))))
   lines))

(ert-deftest test-make-bns ()
  (should (equal (make-bns example) example-bns)))

(defun get-on-counts (bns)
  ;; all this really does is sum all the vectors together
  (let ((on-bits (make-vector (length (car bns)) 0)))
    (dolist (bn bns)
      (dotimes (i (length bn))
        (let ((bit (nth i bn)))
          (if (= bit 1)
              (aset on-bits i (1+ (aref on-bits i)))))))
    on-bits))

(defun bits-to-decimal (bits)
  (binary-string-to-int (string-join (mapcar #'number-to-string bits))))

(defun day3 (lines)
  (let* ((half (/ (length lines) 2))
         (bns (make-bns lines))
         (on-counts (get-on-counts bns))
         (most-common (make-vector (length on-counts) 0))
         (least-common (make-vector (length on-counts) 0)))
    (dotimes (i (length most-common))
      (if (> (aref on-counts i) half)
          (aset most-common i 1)
        (aset least-common i 1)))
    (* (bits-to-decimal most-common) (bits-to-decimal least-common))))

(ert-deftest test-day3 ()
  (should (equal (get-on-counts example-bns) example-on-counts))
  (should (equal (day3 example) example-ans))
  (should (equal (day3 (seq-filter (lambda (line) (> (length line) 0)) (s-lines (f-read "day3.txt")))) 3885894)))

;; (defun gen-fn-for-oxygen (parsed-bns index)
;;   "Determine the /most common/ value in the given bit INDEX, and
;; keep only those PARSED-BNS with that bit in that position.  If
;; both bit values are equally common, keep those PARSED-BNS with a
;; 1 in the INDEX being considered.")

;; (defun gen-fn-for-co2 (parsed-bns index)
;;   "Determine the /least common/ value in the given bit INDEX, and
;; keep only those PARSED-BNS with that bit in that position.  If
;; both bit values are equally common, keep those PARSED-BNS with a
;; 0 in the INDEX being considered.")

;; (defun filter-on-criteria (bits criteria-fn))

;; (defun make-filter-bits-oxygen (half on-counts)
;;   (mapcar (lambda (on-count)
;;             (cond
;;              ((= on-count half) 1)
;;              ((> on-count half) 1)
;;              (t 0)))
;;           on-counts))

;; (defun make-filter-bits-co2 (half on-counts)
;;   (mapcar (lambda (on-count)
;;             (cond
;;              ((= on-count half) 0)
;;              ((> on-count half) 0)
;;              (t 1)))
;;           on-counts))

(defun make-filter-bit-oxygen (half on-count)
  (cond
   ((= on-count half) 1)
   ((> on-count half) 1)
   (t 0)))

(defun make-filter-bit-co2 (half on-count)
  (cond
   ((= on-count half) 0)
   ((> on-count half) 0)
   (t 1)))

(defun filter-on-bits-one-pass (bns)
  (let ((filtered-bns (copy-sequence bns))
        (nbits (length (car bns))))
    (dotimes (bitpos nbits)
      ;; (if (> (length filtered-bns) 1)
          (let* ((half (/ (length filtered-bns) 2))
                 (on-count (apply #'+ (mapcar (lambda (bn) (nth bitpos bn)) filtered-bns)))
                 (filter-bit (make-filter-bit-oxygen half on-count)))
            (princ (format "%d %d\n" on-count filter-bit))
            ;; Remove all binary numbers that don't have the same bit in the
            ;; same position.  If this would remove all remaining binary
            ;; numbers, keep the first one and return.
            (setq possibly-filtered-bns
                  (seq-filter
                   (lambda (bn)
                     (= (nth bitpos bn) filter-bit))
                   filtered-bns))
            (if possibly-filtered-bns
                (setq filtered-bns possibly-filtered-bns)
              (cl-return (car filtered-bns))))
        ;; )      
      (princ (format "%s\n" filtered-bns))
      )
    filtered-bns))

(defun day3-p2 (lines)
  (let* ((half (/ (length lines) 2))
         (bns (make-bns lines))
         (on-counts (get-on-counts bns))
         (most-common (make-vector (length on-counts) 0))
         ;; greater than half, more 1 than 0
         ;; less than half, more 0 than 1
         ;; find most common value
         ;; (filter-bits-oxygen (make-filter-bits-oxygen half on-counts))
         ;; find least common value
         ;; (filter-bits-co2 (make-filter-bits-co2 half on-counts))
         ;; (values-to-keep-oxygen (filter-on-bits bns filter-bits-oxygen))
         ;; (values-to-keep-co2 (filter-on-bits bns filter-bits-co2))
         )
    on-counts))

(ert-deftest test-day3-p2-filter-on-bits ()
  ;; (should (equal (make-filter-bits-oxygen 6 example-on-counts)
  ;;                '(1 0 1 1 0)))
  ;; (should (equal (make-filter-bits-co2 6 example-on-counts)
  ;;                '(0 1 0 0 1)))
  ;; (should (equal (filter-on-bits-one-pass example-bns '(1 0 1 1 0))
  (should (equal (filter-on-bits-one-pass example-bns)
                 '((1 0 1 1 1))))
  ;; (should (equal (filter-on-bits-one-pass example-bns '(0 1 0 0 1))
  ;;                '((0 1 0 1 0))))
  )

;; (ert-deftest test-day3-p2 ()
;;   (should (equal (day3-p2 example) example-ans-p2)))

(let ((lines (seq-filter (lambda (line) (> (length line) 0)) (s-lines (f-read "day3.txt")))))
  ;; (print (day3 lines))
  ;; (print (day3-p2 lines))
  )
