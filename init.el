;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.(package-initialize) ;; You might already have this line

(when (>= emacs-major-version 24)
   (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			      ;; ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			      ;; ("melpa" . "https://elpa.zilongshanren.com/melpa/")
			      ;; ("melpa" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/")
			      ("melpa" . "https://melpa.org/packages/")
			      )))

;; Speed up startup
(defvar centaur-gc-cons-threshold (if (display-graphic-p) 16000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this.If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
  ;; Specify font for all unicode characters
(cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
         when (font-installed-p font)
         return(set-fontset-font t 'unicode font nil 'prepend))

(require 'init-packages)

(require 'init-basic)
(require 'init-locales)

(require 'init-evil)
(require 'init-ui)
(require 'init-hydra)
(require 'init-edit)
(require 'init-ivy)
(require 'init-helm)
(require 'init-company)

(require 'init-dashboard)
(require 'init-dired)

(require 'init-term)

(require 'init-prog)
(require 'init-python)
(require 'init-highlight)
(require 'init-vcs)
(require 'init-elisp)

(require 'init-org)
(require 'init-markdown)

(require 'init-window)
(require 'personal)

;; ----- global keybindings ----- 
(global-set-key (kbd "s-x") #'counsel-M-x)
(global-set-key (kbd "s-/") #'comment-line)
(global-set-key (kbd "s-p") #'helm-mini)
