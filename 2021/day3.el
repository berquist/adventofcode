;; -*- lexical-scope: t -*-

(require 'ert)
(require 'subr-x)
(require 'f)
(require 's)

(defconst example '("00100"
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
(defconst example-ans 22)

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

(defun day3-bits (bns)
  ;; If these are only ever in binary, you really only need one of on or off
  ;; and the total number of entries...
  (let ((on-bits (make-vector (length (car bns)) 0))
        (off-bits (make-vector (length (car bns)) 0)))
    (dolist (bn bns)
      (let* ((entry-bits (mapcar #'string-to-number (mapcar #'char-to-string (string-to-list bn)))))
        (dotimes (i (length entry-bits))
          (let ((bit (nth i entry-bits)))
            (if (= bit 1)
                (aset on-bits i (1+ (aref on-bits i)))
              (aset off-bits i (1+ (aref off-bits i))))))))
    (list on-bits off-bits)))

(defun bits-to-decimal (bits)
  (binary-string-to-int (string-join (mapcar #'number-to-string bits))))

(defun day3 (bns)
  (let* ((half (/ (length bns) 2.0))
         (on-counts (car (day3-bits bns)))
         (most-common (make-vector (length on-counts) 0))
         (least-common (make-vector (length on-counts) 0)))
    (dotimes (i (length most-common))
      (if (> (aref on-counts i) half)
          (aset most-common i 1)
        (aset least-common i 1)))
    (* (bits-to-decimal most-common) (bits-to-decimal least-common))))

(ert-deftest test-day3 ()
  (should (equal (day3-bits example) (list (vector 7 5 8 7 5)
                                           (vector 5 7 4 5 7))))
  (should (equal (day3 example) 198)))

(let ((lines (seq-filter (lambda (line) (> (length line) 0)) (s-lines (f-read "day3.txt")))))
  (print (day3 lines)))
