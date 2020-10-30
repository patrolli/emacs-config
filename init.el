;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.(package-initialize) ;; You might already have this line
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'custom)
(require 'init-org)
(require 'init-keybindings)
(setq ring-bell-function 'ignore)
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load-file custom-file)
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(delete-selection-mode 1)

(put 'dired-find-alternate-file 'disabled nil)

