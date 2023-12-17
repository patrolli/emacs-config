(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Personal information
(setq user-full-name lxs-full-name
      user-mail-address lxs-mail-address)

(if sys/wslp (setq
      cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
      cmdExeArgs '("/c" "start" "chrome") ;; 更新 windows11 后，在 cmd 输入 start url, 只会打开 edge 了，所以这里指定 start chrome
      browse-url-generic-program  cmdExeBin
      browse-url-generic-args     cmdExeArgs
      browse-url-browser-function 'browse-url-generic)
  nil)

;; C-h for delete
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "M-DEL"))
(global-set-key (kbd "<f1>") 'help-command)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; disable bell warning
(setq ring-bell-function 'ignore)

(use-package posframe
  :defer t
  :ensure t)

(use-package openwith
  :hook
  (after-init . openwith-mode)
  :config
  ;; (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" " okular" (file))))
  )

;; 自动保存文件
(use-package auto-save
  :disabled t
  :load-path "site-lisp/auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-idle 2)
  )

;; 试用 super-save 来自动保存
;; super-save 没有 lazycat 的 auto-save 那样激进
;; 它是在 buffer 切换或者失去焦点的时候自动保存
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(defun my--choose-directory ()
  "Return a directory chosen by the user.  The user will be prompted
to choose a directory"
  (let* ((ivy-read-prompt "Choose directory: ")
         (counsel--find-file-predicate #'file-directory-p)
         (selected-directory
          (ivy-read
           ivy-read-prompt
           :matcher #'counsel--find-file-matcher)))
    selected-directory))
(defun lxs-export-org-to-hugo ()
  (interactive)
  (let ((directory (my--choose-directory)))
    (my--export-to-hugo directory)))

;; automatically warp lines
(setq word-wrap-by-category t)
(add-hook 'org-mode-hook '(lambda () (setq visual-line-mode t)))
(add-hook 'prog-mode-hook '(lambda () (setq visual-line-mode t)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
;; (setq global-visual-line-mode t)
;; (setq truncate-lines nil)

(provide 'init-basic)
