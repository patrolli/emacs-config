;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.(package-initialize) ;; You might already have this line

(when (>= emacs-major-version 24)   
   (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			      ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))

;; Speed up startup
(defvar centaur-gc-cons-threshold (if (display-graphic-p) 16000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold centaur-gc-cons-upper-limit
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold centaur-gc-cons-threshold
                  gc-cons-percentage 0.1)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold centaur-gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold centaur-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

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

;; add emoji support
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

(require 'init-packages)

(require 'init-basic)
(require 'init-locales)
(require 'init-input-method)
(require 'init-bookmark)

(require 'init-ui)
(require 'init-hydra)
(require 'init-battery)
(require 'init-edit)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)

(require 'init-dashboard)
(require 'init-dired)
(require 'init-projectile)
(require 'init-persp)
(require 'init-ibuffer)
(require 'init-treemacs)
(require 'init-better-defaults)
(require 'init-utils)

(require 'init-term)


(require 'init-prog)
(require 'init-python)
(require 'init-highlight)
(require 'init-vcs)
(require 'init-elisp)


(require 'init-new-org)
(require 'init-markdown)
(require 'init-reader)

(require 'init-window)
(require 'init-navigate)
(require 'personal)
(require 'init-abbrev)

;; Auto generated by cnfonts
;; <https://github.com/tumashu/cnfonts>
;; (set-face-attribute
;;  'default nil
;;  :font (font-spec :name "-PfEd-DejaVu Sans Mono-bold-oblique-normal-*-*-*-*-*-m-0-iso10646-1"
;;                   :weight 'normal
;;                   :slant 'normal
;;                   :size 12.5))
(set-face-attribute
 'default nil
 :font (font-spec :name "-UKWN-Iosevka-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1"
                  :weight 'normal
                  :slant 'normal
                  :size 13.5))
;; (set-face-attribute
;;  'default nil
;;  :font (font-spec :name "Iosevka"
;;                   :weight 'normal
;;                   :slant 'normal
;;                   :size 14.5))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :name "-ADBO-思源宋体 CN-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"
              :weight 'normal
              :slant 'normal
              :size 12.5)))

;; add emacs in frame title for ahk searching the emacs window
(add-to-list 'frame-title-format "emacs")
