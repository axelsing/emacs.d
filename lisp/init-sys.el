
(prefer-coding-system 'utf-8)

(setq auto-save-default nil	   ; disable auto save
      global-auto-revert-mode t
      auto-window-vscroll nil
      delete-by-moving-to-trash t  ; disable delete directly
      fast-but-imprecise-scrolling t
      frame-title-format "%f"
      help-window-select t
      inhibit-startup-screen t	   ; disable the startup screen splash
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
      use-short-answers t)

(setq-default cursor-type 'bar) 

;; Highlight Current Line
(use-package hl-line
  :when (display-graphic-p)
  :hook (prog-mode . hl-line-mode))

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

(provide 'init-sys)
