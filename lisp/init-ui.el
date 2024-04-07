
(defun available-font (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	      (throw 'font font)))))

(defun cj/setup-font ()
  "Font setup."

  (interactive)
  (let* ((efl '("Hack" "Cascadia Code" "Source Code Pro" "JetBrains Mono" "Courier New" "Monaco" "Ubuntu Mono"))
	 (cfl '("楷体" "黑体" "STHeiti" "STKaiti"))
	 (cf (available-font cfl))
	 (ef (available-font efl)))
    (print efl)
    (print cfl)
    (when ef
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	    (set-face-attribute face nil :family ef)))
    (when cf
      (dolist (charset '(kana han cjk-misc bopomofo))
	    (set-fontset-font t charset cf))
      (setq face-font-rescale-alist
	    (mapcar (lambda (item) (cons item 1.4)) cfl)))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (cabins/setup-font))))
  (add-hook 'after-init-hook #'cj/setup-font))

(provide 'init-ui)
