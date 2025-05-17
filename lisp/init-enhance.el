
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

;; SIDEBAR
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


(use-package flymake
  :disabled)

(use-package flycheck
  :disabled
  :ensure t
  ;; :init (global-flycheck-mode)
  :config
  (setq truncate-lines nil)
  :hook
  (prog-mode . flycheck-mode)
  (c++-mode-hook . (lambda () (setq flycheck-clang-language-standard "c++17"))))

(use-package flycheck-clang-tidy
  :disabled
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

;; 禁用 Flymake 自动加载
(remove-hook 'prog-mode-hook 'flymake-mode)  ; 从编程模式钩子中移除
(remove-hook 'text-mode-hook 'flymake-mode)  ; 从文本模式钩子中移除

;; 如果已加载 Flymake，关闭它
(when (bound-and-true-p flymake-mode)
  (flymake-mode -1))

;; 禁用 Flycheck 自动加载
;(global-unset-key (kbd "C-c !"))  ; 移除 Flycheck 快捷键绑定
(setq flycheck-global-modes nil)  ; 阻止全局模式启用
(remove-hook 'prog-mode-hook 'flycheck-mode)  ; 从编程模式钩子中移除

;; 如果已加载 Flycheck，关闭它
(when (bound-and-true-p flycheck-mode)
  (flycheck-mode -1))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-file-watch-threshold 500)
  (setq lsp-diagnostics-provider :none)  ; 禁用 LSP 诊断
  (setq lsp-enable-diagnostics nil)      ; 确保诊断被禁用
  (setq lsp-prefer-flymake nil)          ; 不使用 Flymake
  ;; LSP headerline 配置
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  ;(setq lsp-headerline-breadcrumb-icons-enable nil)  ; 禁用图标，避免乱码
  :config
  ;; 移除 LSP 对 Flymake/Flycheck 的依赖
  (remove-hook 'lsp-mode-hook #'flymake-mode)
  (remove-hook 'lsp-mode-hook #'flycheck-mode)
  (remove-hook 'lsp-after-open-hook #'flymake-start)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;(c++-mode . lsp)
         ;(c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :bind
  ("C-c l s" . lsp-ivy-workspace-symbol))

;; optionally
(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil)
  (defadvice projectile-project-root (around ignore-remote first activate)
	(unless (file-remote-p default-directory) ad-do-it)))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
		("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

(use-package magit
  :ensure t)


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

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

;; hippie
(global-set-key (kbd "C-<tab>") 'hippie-expand)

;; fzf fuzzy finder
;; Only want to search through git files by default
;; NOTE: Install fzf on your system for this plugin to work

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "~/.fzf/bin/fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;; fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 25))
(global-set-key (kbd "C-x p") 'fzf-git-files)
(global-set-key (kbd "C-x f") 'fzf-git-files)

(use-package rg
  :ensure t)

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

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(global-set-key (kbd "C-j") nil)
(use-package avy
  :ensure t
  :bind
  (("C-j C-SPC" . avy-goto-char-timer)
   ("C-j C-k" . avy-move-line)
   ("C-j C-l" . avy-copy-line)
   ("C-j C-i" . avy-copy-region)))

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-h x" . 'helpful-command))

(use-package hydra
  :demand t
  :ensure t)

(use-package use-package-hydra
  :demand t
  :ensure t
  :after hydra)

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :after hydra
  :bind
  (("C-x C-h m" . hydra-multiple-cursors/body)
   ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
  :hydra
  (hydra-multiple-cursors
   (:hint nil)
   "
Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
   ("l" mc/edit-lines :exit t)
   ("a" mc/mark-all-like-this :exit t)
   ("n" mc/mark-next-like-this)
   ("N" mc/skip-to-next-like-this)
   ("M-n" mc/unmark-next-like-this)
   ("p" mc/mark-previous-like-this)
   ("P" mc/skip-to-previous-like-this)
   ("M-p" mc/unmark-previous-like-this)
   ("|" mc/vertical-align)
   ("s" mc/mark-all-in-region-regexp :exit t)
   ("0" mc/insert-numbers :exit t)
   ("A" mc/insert-letters :exit t)
   ("<mouse-1>" mc/add-cursor-on-click)
   ;; Help with click recognition in this hydra
   ("<down-mouse-1>" ignore)
   ("<drag-mouse-1>" ignore)
   ("q" nil)))

(use-package tiny ; m1\n10|int func%02d ()
  :ensure t)

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Coding is happening")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 20)
						  (bookmarks . 10)
						  (projects . 10)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "/opt/local/bin/pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode)))

(use-package htmlize
  :ensure t
  :defer t)

;; C/C++
(use-package cc-mode
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state)
  :bind
  ("C-c o" . ff-find-other-file)
  ;; ("C-c o" . ff-find-other-file-other-window)
)

;;; Switch between .hh and .cc files
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))


(use-package clang-format
  :ensure t
  :bind
  ("C-c C-f" . clang-format-buffer)
  )

(use-package cmake-mode
  :ensure t)

(provide 'init-enhance)
