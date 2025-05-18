
;; TRAMP 配置
(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  ;(setq tramp-verbose 6)
  (setq tramp-verbose 3)  ; 减少冗长日志
  (setq tramp-copy-size-limit 10000000)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%C' -o ControlPersist=no")
  
  ;; 禁用远程备份文件
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (file-remote-p name))))))

;; 远程终端支持
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  (defun my-remote-term ()
    (interactive)
    (if (file-remote-p default-directory)
        (let ((tramp-file-name (tramp-dissect-file-name default-directory)))
          (multi-term)
          (term-send-string
           (get-buffer-process (current-buffer))
           (format "ssh %s@%s\n"
                   (tramp-file-name-user tramp-file-name)
                   (tramp-file-name-host tramp-file-name))))
      (multi-term))))

(provide 'init-tramp)
