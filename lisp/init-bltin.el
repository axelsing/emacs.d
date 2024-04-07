
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

(provide 'init-bltin)
