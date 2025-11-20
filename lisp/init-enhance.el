
(when (eq system-type 'windows-nt)
;(when nil
  ;; 设置Git Bash为默认shell
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (setq explicit-bash.exe-args '("--login" "-i"))
  
  ;; 添加Git Bash的bin目录到exec-path
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
  
  ;; 设置环境变量
  (setenv "SHELL" shell-file-name)
  (setenv "PATH" (concat "C:/Program Files/Git/bin;"
                         "C:/Program Files/Git/usr/bin;"
                         (getenv "PATH")))

  (defun wsl-path (path)
    "将Windows路径转换为Unix风格路径"
    (when path
      (replace-regexp-in-string "\\\\" "/"
                                 (replace-regexp-in-string "^\\([A-Za-z]\\):" "/mnt/\\L\\1" path))))
  
  ;; 配置shell模式
  (add-hook 'shell-mode-hook
            (lambda ()
              (setq comint-process-echoes t)
              (setq comint-prompt-read-only t)
              (ansi-color-for-comint-mode-on)
              (setq buffer-file-coding-system 'utf-8-unix)))
  )


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
  :ensure t
  :config (windmove-default-keybindings))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
;(use-package exec-path-from-shell
;  :ensure t
;  :when (or (memq window-system '(mac ns x))
;	    (unless cj-os-win
;	      (daemonp)))
;  :init (exec-path-from-shell-initialize))

(use-package crux
    :ensure t
    :bind (;("C-a" . crux-move-beginning-of-line)  ; 智能行首（替代原生 C-a）
         ("C-S-o" . crux-smart-open-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c r" . crux-rename-file-and-buffer)))

(use-package comment-dwim-2
  :ensure t
  :config
  (setq comment-dwim-2-maybe-stay-on-line t)  ; 连续注释时留在当前行
  (setq comment-dwim-2-suppress-newline t)     ; 支持嵌套注释
  :bind (("M-;" . comment-dwim-2)))

;; SIDEBAR
(use-package sr-speedbar
  :disabled
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
  ;(setq lsp-enable-symbol-highlighting t)
  (setq lsp-diagnostics-provider :none)  ; 禁用 LSP 诊断
  (setq lsp-enable-diagnostics nil)      ; 确保诊断被禁用
  (setq lsp-prefer-flymake nil)          ; 使用 flycheck 而非 flymake
  ;; LSP headerline 配置
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  ;(setq lsp-headerline-breadcrumb-icons-enable nil)  ; 禁用图标，避免乱码
  :config
  ;; 支持 TRAMP 远程开发
  (setq lsp-enable-file-watchers nil)  ; 禁用文件监控（远程可能有问题）
  (setq lsp-keep-workspace-alive nil)   ; 关闭项目时自动关闭 LSP 服务器
  ;(setq lsp-auto-guess-root nil)        ; 不自动猜测项目根目录
  ;; 移除 LSP 对 Flymake/Flycheck 的依赖
  (remove-hook 'lsp-mode-hook #'flymake-mode)
  (remove-hook 'lsp-mode-hook #'flycheck-mode)
  (remove-hook 'lsp-after-open-hook #'flymake-start)

  ;; 注册常用远程 LSP 客户端
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                     :major-modes '(python-mode)
                     :remote? t
                     :server-id 'pylsp-remote))

  (lsp-register-client
   (make-lsp-client
    ;:new-connection (lsp-tramp-connection "clangd")
    :new-connection (lsp-tramp-connection
                    (lambda ()
                      (cons "clangd" '("--background-index"
                                       "--compile-commands-dir=build"
                                       "--header-insertion-decorators=0"
                                       "--all-scopes-completion"
                                       "--pch-storage=memory")))) ; "--log=error"; 减少日志输出
    :major-modes '(c-mode c++-mode)
    :remote? t
    :server-id 'clangd-remote))
  ;; 快速打开远程项目
  (defun my-open-remote-project ()
    (interactive)
    (let ((remote-path (read-file-name "Remote path: " "/ssh:")))
      (find-file remote-path)
      (when (projectile-project-p)
        (projectile-add-known-project remote-path))))
  
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
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)  ; 减少干扰
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)     ; 文档显示位置 'bottom at-point
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  ;; 启用 LSP 客户端与服务器的 IO 通信日志（核心日志开关）
  (setq lsp-log-io t)  ; 日志会输出到 *lsp-log* 缓冲区
  (require 'dap-gdb-lldb)
  ;; 配置 GDB 路径（远程）
  (setq dap-gdb-lldb-debug-program '("gdb" "--interpreter=mi"))
  (dap-register-debug-provider "gdb"
    (lambda (conf)
      (-> conf
          (plist-put :type "gdb")
          (plist-put :request "launch")
          (plist-put :name "Debug Remote")))))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  ;(setq projectile-indexing-method 'hybrid)  ; 索引方法
  (setq projectile-enable-caching t)       ; 启用缓存
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically t)
  (setq projectile-completion-system 'ivy)
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
  ;; 宽度和位置配置
  ;(setq treemacs-width 30)               ; 默认宽度
  ;(setq treemacs-position 'right)        ; 放在右侧
  ;; 自动调整宽度（可选）
  (setq treemacs-width-is-initially-locked nil)  ; 允许动态调整
  (setq treemacs-width (if (< (display-pixel-width) 1600) 25 35))
  
  (treemacs-tag-follow-mode t)
  (treemacs-follow-mode t)  ; 自动跟随当前文件
  (treemacs-project-follow-mode t)  ; 跟随项目切换
  (treemacs-filewatch-mode t)  ; 监听文件变化
  (treemacs-git-mode 'deferred)  ; 显示 Git 状态
  (setq treemacs-remote-file-behavior 'copy) ; 支持远程文件树
  (treemacs-resize-icons 16)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;;("C-x t C-t" . treemacs-find-file)
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

; clang.exe -v -E -x c++ -  # 查看默认包含路径
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
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-show-numbers t
        company-tooltip-limit 20
        company-idle-delay 0.2
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
  :ensure t)
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
  (setq ivy-initial-inputs-alist nil)  ; 禁用默认输入
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
  :bind (("C-s" . my-swiper-at-point)
         ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t)
  (defun my-swiper-at-point ()
    (interactive)
    (let ((current-word (if (thing-at-point 'word)
                            (thing-at-point 'word)
                          "")))
      (swiper current-word)))
  )


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

;; sth. like vim easymotion
(global-set-key (kbd "C-j") nil)
(use-package avy
  :ensure t
  :config
  (setq avy-all-windows nil)          ; 只在当前窗口标记
  (setq avy-background t)             ; 背景字符半透明
  (setq avy-style 'at-full)           ; 标记样式：完整单词高亮
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ; 使用 home 行字符作为标记, 最方便按的键
  ;; (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  :bind
  (("C-'" . avy-goto-char)      ; 跳转到字符
   ("C-\"" . avy-goto-char-2)    ; 跳转到双字符组合
   ("M-g f" . avy-goto-line)     ; 跳转到行
   ("M-g w" . avy-goto-word-1)   ; 跳转到单词
   ("M-g e" . avy-goto-word-0) ; 跳转到单词（词尾）
   ("C-j C-SPC" . avy-goto-char-timer)
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
  (("C-S-c C-S-c" . mc/edit-lines)       ; 编辑选中的行
   ("C-c l" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)      ; 标记下一个匹配项
   ("C-<" . mc/mark-previous-like-this)  ; 标记上一个匹配项
   ("C-c C-<" . mc/mark-all-like-this)   ; 标记所有匹配项
   ("C-S-<mouse-1>" . mc/toggle-cursor-on-click) ; 鼠标点击添加光标
   ("C-c m" . hydra-multiple-cursors/body))
  :hydra
  (hydra-multiple-cursors
   (:hint nil)
   "
 多行编辑: [_l_] Edit lines [_n_] 下一个  [_p_] 上一个  [_a_] 所有  [_s_] 搜索  [_q_] 退出
 列编辑:   [_v_] 垂直对齐  [_i_] 插入数字  [_A_] 插入字母
 操作:     [_d_] 删除行  [_b_] 复制行  [_j_] 合并行"
   ("l" mc/edit-lines :exit t)
   ("n" mc/mark-next-like-this)
   ("p" mc/mark-previous-like-this)
   ("a" mc/mark-all-like-this)
   ("s" mc/mark-all-in-region-regexp)
   ("v" mc/vertical-align)
   ("i" mc/insert-numbers)
   ("A" mc/insert-letters)
   ("d" mc/delete-lines)
   ("b" mc/duplicate-current-line-or-region)
   ("j" mc/join-lines)
   ("q" nil :color blue)))

;; 智能扩大选区
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Coding is happening")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((projects . 10)
                          (recents  . 20)
						  (bookmarks . 10)
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; 调整样式
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))
;(dashboard-setup-startup-hook) not work
(add-hook 'after-init-hook 'dashboard-open)  ; 启动后强制打开

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

;; https://google.github.io/styleguide/cppguide.html#Formatting
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;;; Switch between .hh and .cc files
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; 确保 C++ 模式下换行缩进使用 cc-mode 自带函数
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             ;; 绑定 Enter 键到 newline-and-indent（默认已绑定，但需确认）
;;             (local-set-key (kbd "RET") 'newline-and-indent)
;;             ;; 确保缩进函数为 cc-mode 自带
;;             (setq-local indent-line-function 'c-indent-line)))

;; 解决 {|} 回车后indent错误
(defun my-clang-format-sync-cc-mode ()
  "同步 clang-format 配置到 cc-mode 变量"
  (interactive)
  ;; 查找项目根目录的 .clang-format 文件
  (let* ((clang-format-file (locate-dominating-file default-directory ".clang-format"))
         (indent-width 2)  ; 默认值，若找不到 .clang-format 则使用
         (brace-offset 0)) ; 大括号偏移量（默认 0）
    ;; 解析 .clang-format 中的 IndentWidth
    (when clang-format-file
      (with-temp-buffer
        (insert-file-contents clang-format-file)
        ;; 提取 IndentWidth（正则匹配 "IndentWidth: 4" 类似行）
        (goto-char (point-min))
        (when (re-search-forward "IndentWidth:\\s-+\\([0-9]+\\)" nil t)
          (setq indent-width (string-to-number (match-string 1))))
        ;; 提取 BraceWrapping 相关偏移（可选，根据需要添加）
        )))
  ;; 同步到 cc-mode 变量
  (setq-local c-basic-offset indent-width)
  (setq-local c-indent-level indent-width)
  (setq-local c-continued-statement-offset indent-width)
  (setq-local c-brace-offset brace-offset))

;; 在 C/C++ 模式启动时自动同步 clang-format indent-width
(add-hook 'c-mode-hook 'my-clang-format-sync-cc-mode)
(add-hook 'c++-mode-hook 'my-clang-format-sync-cc-mode)

(use-package clang-format
  :ensure t
  :bind
  ("C-c C-f" . clang-format-buffer)
)

(use-package cmake-mode
  :ensure t)

(provide 'init-enhance)
