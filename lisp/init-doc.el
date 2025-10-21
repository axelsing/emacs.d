
(setq org-directory (file-truename "f:/org/"))
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq cj/org-refile-file (concat org-directory "refile.org"))
(setq cj/org-agenda-files `(,(concat org-directory "agenda/")))

(use-package org
  :pin nongnu
  :mode (("\\.org\\'" . org-mode))
  :ensure org-contrib
  :init
  (require 'org-indent)
  :custom
  (org-ellipsis " ▾")
  (org-adapt-indentation t)
  (org-log-done t)
  (org-src-tab-acts-natively nil)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-folde>d t)
  (org-startup-indented t)
  (org-startup-with-inline-images t "always display inline image")
  (org-image-actual-width 600 "set width of image when displaying")
  (org-hide-leading-stars t "clearer way to display")
  
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		   (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))
  (org-todo-keyword-faces
   (quote (("TODO" :foreground "goldenrod1" :weight bold)
		   ("NEXT" :foreground "DodgerBlue1" :weight bold)
		   ("DONE" :foreground "SpringGreen2" :weight bold)
		   ("WAITING" :foreground "LightSalmon1" :weight bold)
		   ("CANCELLED" :foreground "LavenderBlush4" :weight bold)
		   ("MEETING" :foreground "IndianRed1" :weight bold))))
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
		   ("WAITING" ("WAITING" . t))
		   (done ("WAITING"))
		   ("TODO" ("WAITING") ("CANCELLED"))
		   ("NEXT" ("WAITING") ("CANCELLED"))
		   ("DONE" ("WAITING") ("CANCELLED")))))
  (org-capture-templates
   (quote (("t" "todo" entry (file cj/org-refile-file)
			"* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		   ("r" "respond" entry (file cj/org-refile-file)
			"* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
		   ("n" "note" entry (file cj/org-refile-file)
			"* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		   ("w" "org-protocol" entry (file cj/org-refile-file)
			"* TODO Review %c\n%U\n" :immediate-finish t)
		   ("m" "Meeting" entry (file cj/org-refile-file)
			"* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))
  
  (org-agenda-files cj/org-agenda-files)
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-compact-blocks t)
  (org-agenda-span 7)
  (org-agenda-start-day "-2d")
  (org-agenda-start-on-weekday nil)
  (org-agenda-tags-column -86)
  (org-refile-targets (quote ((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9))))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes (quote confirm))
  
  :config
  ;; 自动换行
  (setq word-wrap nil)
  (setq word-wrap-by-category t)
  (setq truncate-lines nil)
  (setq toggle-truncate-lines t)

  ;; 代码块设置
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (latex . t)))

   ;; 导出设置
  (setq org-export-with-smart-quotes t)
  (setq org-export-with-toc t)
  (setq org-export-with-section-numbers nil)

  :bind
  (("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag))

  :hook
  (org-mode . display-line-numbers-mode)
)

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

;; 美化显示
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

; 笔记网络
(use-package org-roam
  :pin melpax
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :config
  (org-roam-setup)
  :custom
  (org-roam-directory (concat org-directory "roam/")) ; 设置 org-roam 目录
  :bind
  (("C-c n f" . org-roam-node-find)
  (:map org-mode-map
     (("C-c n i" . org-roam-node-insert)
      ("C-c n o" . org-id-get-create)
      ("C-c n t" . org-roam-tag-add)
      ("C-c n a" . org-roam-alias-add)
      ("C-c n l" . org-roam-buffer-toggle)))))

; 每日日记
(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir (concat org-directory "journal/")
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y-%m-%d.org")
  :bind
  (("C-c n j" . org-journal-new-entry)))

; 演示模式
(use-package org-tree-slide
  :ensure t
  :config
  (setq org-tree-slide-activate-message "start slide!")
  (setq org-tree-slide-deactivate-message "end slide!"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "d:/tools/pandoc-3.2/pandoc.exe")
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode)))

(use-package htmlize
  :ensure t
  :defer t)

(provide 'init-doc)
