
(defmacro cabins/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(provide 'init-fn)
