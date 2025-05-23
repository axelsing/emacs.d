;; 最小配置测试
(package-initialize)
(require 'company)
(global-company-mode 1)
(setq company-backends '(company-capf company-clang))


;D:\tools\emacs-29.4\bin\runemacs.exe -q -l C:/Users/void/AppData/Roaming/.emacs.d/minimal-config.el
;toggle-debug-on-error