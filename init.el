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
;;(require 'init-input-method)
(require 'init-utils)

(require 'init-ui)
;; (require 'init-hydra)
(require 'init-edit)
(require 'init-ivy)
;; (require 'init-consult)
(require 'init-company)
(require 'init-yasnippet)

(require 'init-dashboard)
;; (require 'init-dired)
;; (require 'init-projectile)
;; (require 'init-better-defaults)

(require 'init-term)

(require 'init-prog)
(require 'init-python)

;(require 'init-highlight)		
(require 'init-vcs)
;(require 'init-elisp)

;; (require 'init-org)
;; ;; (require 'init-org-clock)
;; (require 'init-org-utils)
(require 'init-markdown)
(require 'init-blog)
;; (require 'init-reader)

(require 'init-window)
;; (require 'personal)
;; (require 'init-abbrev)
;(require 'init-flycheck)
;; (require 'init-term-cursor)
;; ;; (require 'init-restore)

;; 暂时停用 desktop mode
;; 在启动时选择手动恢复会话，而不是自动恢复
;; 现在还不支持手动，后面在 dashboard 加入 restore 的选项
;; (require 'init-restore)


;; (set-face-attribute
 ;; 'default nil
 ;; :font (font-spec :name "-WenQ-WenQuanYi Zen Hei Mono-medium-normal-normal-*-19-*-*-*-*-0-iso10646-1"
                  ;; :weight 'normal
                  ;; :slant 'normal
                  ;; :size 14.5))
;; -WQYF-WenQuanYi Micro Hei Mono-regular-normal-normal-*-17-*-*-*-*-0-iso10646-1
;; (set-face-attribute
;;  'default nil
;;  :font (font-spec :name "Iosevka"
;;                   :weight 'normal
;;                   :slant 'normal
;;                   :size 14.5))

;;  (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;    (set-fontset-font
;;     (frame-parameter nil 'font)
;;     charset
;;     (font-spec :name "-ADBO-思源宋体 CN-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"
;;                :weight 'normal
;;                :slant 'normal
;;               :size 12.5)))
(use-package evil
  :ensure t
  :config
    (evil-mode 1)
    ;; better move around visual lines
;;     (defun evil-next-line--check-visual-line-mode (orig-fun &rest args)
;; 	(if visual-line-mode
;; 	    (apply 'evil-next-visual-line args)
;; 	    (apply orig-fun args)))
;;     (advice-add 'evil-next-line :around 'evil-next-line--check-visual-line-mode)
;;     (defun evil-previous-line--check-visual-line-mode (orig-fun &rest args)
;; 	(if visual-line-mode
;; 	    (apply 'evil-previous-visual-line args)
;; 	    (apply orig-fun args)))
;;     (advice-add 'evil-previous-line :around 'evil-previous-line--check-visual-line-mode)
)


(use-package helm
  :ensure t
  :config
  ;; (setq helm-mini-default-sources
  ;; 	'(helm-source-recentf helm-source-buffers-list helm-source-buffer-not-found helm-source-projectile-files-list))
  ;; default
  (setq helm-mini-default-sources
	'(helm-source-bookmarks helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found))
  ; 单独的 frame 显示 helm 搜索
  (setq helm-display-function 'helm-default-display-buffer))

(use-package helm-projectile
  :ensure t)

(use-package helm-rg
  :ensure t)

;(global-set-key (kbd "s-x") #'counsel-M-x)
;(global-set-key (kbd "s-/") #'comment-line)
;(global-set-key (kbd "s-p") #'helm-mini)

(defun xs-open-inbox-md ()
  (interactive)
  (find-file "C:/Users/xunsong.li/Documents/org/denotes/daily-inbox.md")
  )

;; for windows, bind leader key to ctrl, which needs to
;; overwrite the evil-mode keymappings
(define-key evil-normal-state-map (kbd "C-p") 'helm-mini)
(define-key evil-insert-state-map (kbd "C-p") 'helm-mini)
(define-key evil-normal-state-map (kbd "C-/") 'comment-line)
(define-key evil-insert-state-map (kbd "C-/") 'comment-line)
(define-key evil-visual-state-map (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-s") #'save-buffer)
(global-set-key (kbd "<f1>") #'xs-open-inbox-md)

(use-package denote
  :ensure t
  :config
  ;; This directory is temp, just for mt windows computer
  (setq denote-directory (file-name-concat "c:/Users/xunsong.li/" "Documents" "org" "denotes")
	denote-file-type 'markdown-yaml))

(put 'dired-find-alternate-file 'disabled nil)
(setq native-comp-async-report-warnings-errors nil)

(use-package cnfonts
  :ensure t
  :config
  (cnfonts-set-font))

(setq make-backup-files nil)
