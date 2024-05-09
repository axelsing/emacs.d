
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

(add-hook 'prog-mode-hook 'column-number-mode) ;在ModeLine显示列号
;(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;显示行号
(add-hook 'prog-mode-hook 'electric-pair-mode) ;括号的配对
;(add-hook 'prog-mode-hook 'flymake-mode) ;错误的提示
(add-hook 'prog-mode-hook 'hs-minor-mode) ;代码的折叠
(add-hook 'prog-mode-hook 'prettify-symbols-mode) ;会将lambda等符号美化为λ

(provide 'init-bltin)
