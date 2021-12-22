;; -*- lexical-scope: t -*-

(require 'ert)
(require 'f)
(require 's)

(defconst example
  '(199 200 208 210 200 207 240 269 260 263))
(defconst example-ans 7)

(defun day1 (depths)
  (let ((previous (car depths))
        (count 0))
    (dolist (current (cdr depths))
      (if (> current previous)
          (setq count (1+ count)))
      (setq previous current))
    count))

(ert-deftest test-day1 ()
  (should (equal (day1 example) example-ans)))

(princ (day1 (mapcar #'string-to-number (s-lines (f-read "day1.txt")))))
