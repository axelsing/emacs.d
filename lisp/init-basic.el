
(defmacro cj/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar cj-os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cj-os-mac (eq system-type 'darwin))

(prefer-coding-system 'utf-8)

(setq auto-save-default nil	   ; disable auto save
      global-auto-revert-mode t
      auto-window-vscroll nil
      delete-by-moving-to-trash t  ; disable delete directly
      fast-but-imprecise-scrolling t
      frame-title-format "%f" ; %f - file, %b - buffer
      help-window-select t
      inhibit-startup-screen t	   ; disable the startup screen splash
      ;inhibit-startup-echo-area-message t  ; 底部启动信息
      inhibit-default-init t
      ;; initial-scratch-message nil
      inhibit-compacting-font-caches t
      initial-major-mode 'fundamental-mode
      make-backup-files nil             ; disable backup file
      ;; Mouse wheel scroll behavior
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 1
      scroll-down-aggressively 1
      ;;
      visible-bell nil
      ;default-text-properties '(line-spacing 0.2 line-height 1.2) ;default line height
      isearch-allow-motion t
	  isearch-lazy-count t
	  kill-whole-line t
	  mode-line-compact t
      use-short-answers t
      
      toggle-debug-on-error t
      display-line-numbers t
      display-line-numbers t
      )

(setq-default cursor-type 'bar)

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;(window-number-mode 1)          ; 为窗口添加编号
;(tab-bar-mode 1)                ; 启用标签栏

;;; macOS
;; move file to trash when delete
(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t))

;; <macOS> Command -> Meta, Option -> Super
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

;;; Windows
;; spcial coding settings for Windows
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (setq selection-coding-system 'utf-8))

(require 'init-elpa)

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
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;显示行号
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
;(global-set-key (kbd "RET") 'newline-and-indent)

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
;(global-set-key (kbd "C-a") 'back-to-indentation) ;; swap C-a and M-m
;(global-set-key (kbd "M-m") 'move-beginning-of-line)
;;(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-h") 'hs-hide-block)
(global-set-key (kbd "H-s") 'hs-show-block)

(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

;; bookmark
(global-set-key (kbd "H-x m") 'bookmark-set)
(global-set-key (kbd "H-x b") 'bookmark-jump)
(global-set-key (kbd "H-x l") 'bookmark-bmenu-list)

(global-set-key (kbd "C-c r") 'recentf-open-files) ; Open Recent Files
(global-set-key (kbd "C-x C-r") 'recentf-open-files) ; Open Recent Files

;; scroll up/down
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(global-set-key [C-up] 'scroll-down-in-place)
(global-set-key [C-down] 'scroll-up-in-place)

(global-set-key (quote [M-down]) (quote scroll-up-line))
(global-set-key (quote [M-up]) (quote scroll-down-line))

;(smartscan-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun available-font (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	      (throw 'font font)))))

(defun cj/setup-font ()
  "Font setup."

  (interactive)
  (let* ((efl '("Hack Nerd Font" "Hack" "Cascadia Code" "Source Code Pro" "JetBrains Mono" "Courier New" "Monaco" "Ubuntu Mono"))
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
	    (mapcar (lambda (item) (cons item 1.2)) cfl)))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (cj/setup-font))))
  (add-hook 'after-init-hook #'cj/setup-font))

;(setq custom-safe-themes t)
;(use-package zenburn-theme
;  :ensure t
;  :init
;  (load-theme 'zenburn t))

;;(use-package nimbus-theme
;;  :ensure t
;;  :config
;;  (load-theme 'nimbus t))

(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package emacs
  :ensure nil
  ;:bind (("C--" . text-scale-decrease)
  ;       ("C-=" . text-scale-increase)
  ;       ("C-0" . text-scale-adjust))
  :config
  (set-face-attribute 'default nil :height 150))
  ;(set-face-attribute 'default nil :family "Hack Nerd Font" :height 120))

(eval-when-compile
  (use-package powerline))

;; hs like org-mode
(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

(defun hs-cycle-key-bindings ()
  (keymap-set hs-minor-mode-map "C-<tab>" 'hs-cycle)
  (keymap-set hs-minor-mode-map "C-S-<tab>" 'hs-global-cycle))
(add-hook 'hs-minor-mode-hook 'hs-cycle-key-bindings)


(provide 'init-basic)
