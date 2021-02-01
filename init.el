;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.(package-initialize) ;; You might already have this line
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/nox")
(add-to-list 'load-path "~/.emacs.d/iscroll")
(add-to-list 'load-path "~/.emacs.d/watch-other-window")
(add-to-list 'load-path "~/.emacs.d/alert-toast")
(add-to-list 'load-path "~/.emacs.d/auto-save")

;; emacs garbage configurations
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  #x40000000) ; 1G
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 15 nil #'my-cleanup-gc)

;; default
(setq ring-bell-function 'ignore)

(setq lxs/home-dir "/mnt/c/Users/lixun/")
(setq-default sysTypeSpecific  system-type) 
(cond 
  ;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
  ((eq sysTypeSpecific 'gnu/linux)  
   (when (string-match "Linux.*Microsoft.*Linux" 
                       (shell-command-to-string "uname -a"))
 
     (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
     (setq
      cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
      cmdExeArgs '("/c" "start" "") )
     (setq
      browse-url-generic-program  cmdExeBin
      browse-url-generic-args     cmdExeArgs
      browse-url-browser-function 'browse-url-generic)
     )))

(require 'init-packages)
(require 'init-ui)
(require 'init-org)
(require 'init-better-defaults)
(require 'custom)
(require 'init-keybindings)
(require 'init-prog)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-markdown)
(require 'init-treemacs)
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load-file custom-file)
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;(add-hook 'after-init-hook #'fancy-qbattery-mode)
(delete-selection-mode 1)

(put 'dired-find-alternate-file 'disabled nil)

