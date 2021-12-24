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
  (let* ((half (/ (length lines) 2.0))
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
  ;; (should (equal (day3 (seq-filter (lambda (line) (> (length line) 0)) (s-lines (f-read "day3.txt")))) 3885894))
  )

(defun get-on-count (bns bitidx)
  (apply #'+ (mapcar (lambda (bn) (nth bitidx bn)) bns)))

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

(defun filter-on-bit-pass (bns bitidx make-filter-bit-fn)
  (let* ((half (/ (length bns) 2.0))
         (on-count (get-on-count bns bitidx))
         (filter-bit (funcall make-filter-bit-fn half on-count))
         (filtered-bns (seq-filter (lambda (bn) (= (nth bitidx bn) filter-bit)) bns)))
    filtered-bns))

(defun filter-on-bit (bns bitidx make-filter-bit-fn)
  ;; This assumes that a pass will never go from more than one binary number
  ;; down to zero.
  (if (= (length bns) 1)
      (car bns)
    (let ((filtered-bns (filter-on-bit-pass bns bitidx make-filter-bit-fn)))
      (filter-on-bit
       filtered-bns (1+ bitidx) make-filter-bit-fn))))

(defun filter-on-bits (bns make-filter-bit-fn)
  (filter-on-bit bns 0 make-filter-bit-fn))

(defun day3-p2 (lines)
  (let* ((bns (make-bns lines))
         (oxygen-bn (filter-on-bits bns #'make-filter-bit-oxygen))
         (co2-bn (filter-on-bits bns #'make-filter-bit-co2)))
    (* (bits-to-decimal oxygen-bn) (bits-to-decimal co2-bn))))

(ert-deftest test-day3-p2-filter-on-bits ()
  (should (equal (filter-on-bit-pass
                  example-bns
                  0
                  #'make-filter-bit-oxygen)
                 '((1 1 1 1 0)
                   (1 0 1 1 0)
                   (1 0 1 1 1)
                   (1 0 1 0 1)
                   (1 1 1 0 0)
                   (1 0 0 0 0)
                   (1 1 0 0 1))))
  (should (equal (get-on-counts '((1 1 1 1 0)
                                  (1 0 1 1 0)
                                  (1 0 1 1 1)
                                  (1 0 1 0 1)
                                  (1 1 1 0 0)
                                  (1 0 0 0 0)
                                  (1 1 0 0 1)))
                 (vector 7 3 5 3 3)))
  (should (equal (get-on-count
                  '((1 1 1 1 0)
                    (1 0 1 1 0)
                    (1 0 1 1 1)
                    (1 0 1 0 1)
                    (1 1 1 0 0)
                    (1 0 0 0 0)
                    (1 1 0 0 1))
                  1)
                 3))
  (should (equal (make-filter-bit-oxygen 3.5 3) 0))
  (should (equal (filter-on-bit-pass
                  '((1 1 1 1 0)
                    (1 0 1 1 0)
                    (1 0 1 1 1)
                    (1 0 1 0 1)
                    (1 1 1 0 0)
                    (1 0 0 0 0)
                    (1 1 0 0 1))
                  1
                  #'make-filter-bit-oxygen)
                 '((1 0 1 1 0)
                   (1 0 1 1 1)
                   (1 0 1 0 1)
                   (1 0 0 0 0))))
  (should (equal (filter-on-bit-pass
                  '((1 0 1 1 0)
                    (1 0 1 1 1)
                    (1 0 1 0 1)
                    (1 0 0 0 0))
                  2
                  #'make-filter-bit-oxygen)
                 '((1 0 1 1 0)
                   (1 0 1 1 1)
                   (1 0 1 0 1))))
  (should (equal (filter-on-bit-pass
                  '((1 0 1 1 0)
                    (1 0 1 1 1)
                    (1 0 1 0 1))
                  3
                  #'make-filter-bit-oxygen)
                 '((1 0 1 1 0)
                   (1 0 1 1 1))))
  (should (equal (filter-on-bit-pass
                  '((1 0 1 1 0)
                    (1 0 1 1 1))
                  4
                  #'make-filter-bit-oxygen)
                 '((1 0 1 1 1))))
  (should (equal (filter-on-bit example-bns 0 #'make-filter-bit-oxygen)
                 '(1 0 1 1 1)))
  (should (equal (filter-on-bits example-bns #'make-filter-bit-oxygen)
                 '(1 0 1 1 1)))
  (should (equal (filter-on-bits example-bns #'make-filter-bit-co2)
                 '(0 1 0 1 0))))

(ert-deftest test-day3-p2 ()
  (should (equal (day3-p2 example) example-ans-p2)))

(let ((lines (seq-filter (lambda (line) (> (length line) 0)) (s-lines (f-read "day3.txt")))))
  (print (day3 lines))
  (print (day3-p2 lines)))
