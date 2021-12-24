(defun run-binary-func (fn arg1 arg2)
  (funcall fn arg1 arg2))

(print (run-binary-func #'+ 3 4))
