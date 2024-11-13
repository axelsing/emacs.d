(require 'tramp)

(setq putty-directory "C:\\Program Files\\PuTTY")
(when (eq window-system 'w32)
  (setq tramp-default-method "ssh")
  (setq tramp-completion-without-shell-p t)
  (setq tramp-verbose 10)
  (setq tramp-debug-buffer t)
  ;(with-eval-after-load 'tramp (tramp-change-syntax 'simplified))
  ;(with-eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;;(setq tramp-default-user "chengjie2")
  (when (and (not (string-match putty-directory (getenv "PATH")))
             (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
    (add-to-list 'exec-path putty-directory)))

(add-to-list 'tramp-restricted-shell-hosts-alist "hyp")
(add-to-list 'tramp-default-proxies-alist
             '("10.132.4.55" nil "/plinkx:hyp:"))

;(print tramp-restricted-shell-hosts-alist)
;(print tramp-default-proxies-alist)

(defun remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host :"))))
      (cd (concat "/plinkx:" host ":"))
      (shell (concat "*" host "*")))))

(defun win-tramp-using-ssh (host)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote host)
                     "login-program" "ssh"))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote host)
                     "login-args" '(("%h")))))

(win-tramp-using-ssh "10.132.4.55")  ;; A30
(win-tramp-using-ssh "10.252.3.94")  ;; A10
(win-tramp-using-ssh "10.81.8.5") ;; L20

;(print tramp-connection-properties)

(defun taf_16 ()
  (interactive)
  (remote-shell "taf_16"))

(defun taf_20 ()
  (interactive)
  (remote-shell "taf_20"))

(defun dev_a30 ()
  (interactive)
  (remote-shell "dev_a30"))

(defun dev_a30_x ()
  (interactive)
  (remote-shell "chengjie2@10.132.4.55"))

(defun dev_a10_x ()
  (interactive)
  (remote-shell "chengjie2@10.252.3.94"))

(defun dev_l20_x ()
  (interactive)
  (remote-shell "chengjie2@10.81.8.5"))


(defun huya ()
  (interactive)
  (remote-shell "huya"))


(provide 'init-tramp)
