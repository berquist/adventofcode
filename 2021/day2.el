;; -*- lexical-scope: t -*-

(require 'ert)
(require 'seq)
(require 'f)
(require 's)
(require 'dash)

(defconst example '((forward 5)
                    (down 5)
                    (forward 8)
                    (up 3)
                    (down 8)
                    (forward 2)))
(defconst example-ans 150)

(defun day2 (moves)
  (let ((pos-v 0)
        (pos-h 0))
    (dolist (move moves)
      (let ((dir (car move))
            (amt (cadr move)))
        (cond
         ((string= dir "forward")
          (setq pos-h (+ pos-h amt)))
         ((string= dir "up")
          (setq pos-v (- pos-v amt)))
         ((string= dir "down")
          (setq pos-v (+ pos-v amt))))))
    (* pos-v pos-h)))

(ert-deftest test-day2 ()
  (should (equal (day2 example) example-ans)))

(defun transform-move (move-line)
  (let ((move (s-split " " move-line)))
    `(,(car move) ,(string-to-number (cadr move)))))

(let* ((input (f-read "day2.txt"))
       (lines (seq-filter (lambda (line) (> (length line) 0)) (s-lines input)))
       (transformed-lines (mapcar #'transform-move lines)))
  (print (day2 transformed-lines)))
