;; -*- lexical-scope: t -*-

(require 'ert)
(require 'f)
(require 's)

(defconst example
  '(199 200 208 210 200 207 240 269 260 263))
(defconst example-ans 7)
(defconst example-ans-p2 5)

(defun day1 (depths)
  (let ((count 0)
        (previous (car depths)))
    (dolist (current (cdr depths))
      (if (> current previous)
          (setq count (1+ count)))
      (setq previous current))
    count))

(ert-deftest test-day1 ()
  (should (equal (day1 example) example-ans)))

(defun day1-p2 (depths)
  (let* ((count 0)
         (first (car depths))
         (second (cadr depths))
         (third (caddr depths))
         (previous-sum (+ first second third)))
    (dolist (head (cdddr depths))
      (let* ((cur-first second)
             (cur-second third)
             (cur-third head)
             (cur-sum (+ cur-first cur-second cur-third)))
        (if (> cur-sum previous-sum)
            (setq count (1+ count)))
        (setq first cur-first
              second cur-second
              third cur-third
              previous-sum cur-sum)))
    count))

(ert-deftest test-day1-p2 ()
  (should (equal (day1-p2 example) example-ans-p2)))

(print (day1 (mapcar #'string-to-number (s-lines (f-read "day1.txt")))))
(print (day1-p2 (mapcar #'string-to-number (s-lines (f-read "day1.txt")))))
