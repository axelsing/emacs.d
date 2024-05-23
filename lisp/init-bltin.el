
(use-package display-line-numbers
  :if (> emacs-major-version 26)
  :hook (prog-mode . display-line-numbers-mode))

(add-hook 'after-init-hook (lambda ()
			     (recentf-mode 1)
			     (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/"))))
(setq-default recentf-max-menu-items 50
	      recentf-max-saved-items 50)

(save-place-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;(add-hook 'prog-mode-hook 'column-number-mode) ;在ModeLine显示列号
(add-hook 'prog-mode-hook (lambda () (setq-local column-number-mode t)))
;(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;显示行号
(add-hook 'prog-mode-hook 'electric-pair-mode) ;括号的配对
;(add-hook 'prog-mode-hook 'flymake-mode) ;错误的提示
(add-hook 'prog-mode-hook 'hs-minor-mode) ;代码的折叠
(add-hook 'prog-mode-hook 'prettify-symbols-mode) ;会将lambda等符号美化为λ
(add-hook 'prog-mode-hook 'which-function-mode)

(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(delete-selection-mode t)
(add-to-list 'default-frame-alist '(width . 100))  ; （可选）设定启动图形界面时的初始 Frame 宽度（字符数）
(add-to-list 'default-frame-alist '(height . 55)) ; （可选）设定启动图形界面时的初始 Frame 高度（字符数）

;;; keyboard setting
(global-set-key (kbd "RET") 'newline-and-indent)

;; Set home to Hyper
(global-set-key (kbd "<home>") nil)
(define-key function-key-map (kbd "<home>") 'event-apply-hyper-modifier)

;; Set end to super
(global-set-key (kbd "<end>") nil)
(define-key function-key-map (kbd "<end>") 'event-apply-super-modifier)

;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))

(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "M-w") 'kill-region)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-a") 'back-to-indentation) ;; swap C-a and M-m
(global-set-key (kbd "M-m") 'move-beginning-of-line)
;;(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-h") 'hs-hide-block)
(global-set-key (kbd "H-s") 'hs-show-block)

(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

;; bookmark
(global-set-key (kbd "H-x m") 'bookmark-set)
(global-set-key (kbd "H-x b") 'bookmark-jump)
(global-set-key (kbd "H-x l") 'bookmark-bmenu-list)

(provide 'init-bltin)
