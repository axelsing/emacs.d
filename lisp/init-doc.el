
(setq org-directory (file-truename "f:/org/"))
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
  ;; 自动换行
  (truncate-lines nil)
  (toggle-truncate-lines t)
  :bind
  (("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag))
)

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

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

(provide 'init-doc)
