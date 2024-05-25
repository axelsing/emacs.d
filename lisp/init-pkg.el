
;; Highlight Current Line
(use-package hl-line
  :when (display-graphic-p)
  :hook (prog-mode . hl-line-mode))

;; Show Paren Mode
(use-package paren
  :config
  (setq show-paren-when-point-in-periphery t
	show-paren-when-point-inside-paren t
	show-paren-style 'mixed))

;; windmove.el, use  <SHIFT - arrow key> to switch buffers
(use-package windmove
  :config (windmove-default-keybindings))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
;(use-package exec-path-from-shell
;  :ensure t
;  :when (or (memq window-system '(mac ns x))
;	    (unless cj-os-win
;	      (daemonp)))
;  :init (exec-path-from-shell-initialize))

(use-package crux)

;; Sidebar
(use-package sr-speedbar
  :ensure t
  :config
  (sr-speedbar-open)
  (custom-set-variables
   '(speedbar-show-unknown-files t))
  )


(use-package all-the-icons
  :when (display-graphic-p))

(use-package auto-package-update
  :init (setq auto-package-update-delete-old-versions t
	      auto-package-update-hide-results t))

(use-package which-key
  :defer nil
  :diminish
  :init (which-key-mode))

(use-package smartscan
  :init (smartscan-mode))

;;; All modes
(use-package cuda-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
)

;(use-package bazel-mode
;  :ensure t
;  :init
;  (add-to-list 'auto-mode-alist '("BUILD" . bazel-mode))
;)
(use-package protobuf-mode
  :ensure t)

;;; Switch between .hh and .cc files
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         (c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t))

(use-package company
  :config
  (setq company-dabbrev-code-everywhere t
        company-dabbrev-code-modes t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-other-buffers 'all
        company-require-match nil
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 20
        company-idle-delay 0
        company-echo-delay 0
        company-tooltip-offset-display 'scrollbar
        company-begin-commands '(self-insert-command))
  (push '(company-semantic :with company-yasnippet) company-backends)
  :hook ((after-init . global-company-mode)))

;; fzf fuzzy finder
;; Only want to search through git files by default
;; NOTE: Install fzf on your system for this plugin to work

(use-package fzf
  :ensure t)
(global-set-key (kbd "C-x p") 'fzf-git-files)
(global-set-key (kbd "C-x f") 'fzf-git-files)

;; (use-package ivy
;;   :defer 1
;;   :demand
;;   :hook (after-init . ivy-mode)
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t
;;         ivy-initial-inputs-alist nil
;;         ivy-count-format "%d/%d "
;;         enable-recursive-minibuffers t
;;         ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (search-default-mode #'char-fold-to-regexp)
  (ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-x C-b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))


(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-recentf)
         ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))


(provide 'init-pkg)

