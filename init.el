
(setq default-directory "~/")
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))

(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
	comp-deferred-compilation t
	package-native-compile t)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache" user-emacs-directory)))

(require 'init-fn)
(require 'init-sys)
(require 'init-elpa)
(require 'init-pkg)
(require 'init-bltin)
(require 'init-ui)
(require 'init-kbd)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
