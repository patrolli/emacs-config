;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.(package-initialize) ;; You might already have this line
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/nox")


;; emacs garbage configurations
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  #x40000000) ; 1G
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 15 nil #'my-cleanup-gc)

;; default
(setq ring-bell-function 'ignore)


(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'custom)
(require 'init-org)
(require 'init-keybindings)
(require 'init-prog)
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load-file custom-file)
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(add-hook 'after-init-hook #'fancy-battery-mode)
(delete-selection-mode 1)

(put 'dired-find-alternate-file 'disabled nil)

