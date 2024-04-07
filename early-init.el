
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
			     (setq gc-cons-threshold 800000)))

(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(set-language-environment 'utf-8)

(if (>= emacs-major-version 28)
    (load-theme 'modus-operandi t)
  (load-theme 'leuven t))

(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
