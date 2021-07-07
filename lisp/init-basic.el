(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Personal information
(setq user-full-name lxs-full-name
      user-mail-address lxs-mail-address)

(if sys/wslp (setq
      cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
      cmdExeArgs '("/c" "start" "")
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

(use-package color-rg
  :load-path "site-lisp/color-rg/"
  :init
  (define-key global-map (kbd "C-x l") (make-sparse-keymap))
  :bind
  (("C-x l p" . color-rg-search-project)
   ("C-x l i" . color-rg-search-input)
   ("C-x l s" . color-rg-search-symbol)
   ("C-x l o" . lxs/search-org)
   )
  )

(use-package thing-edit
  :load-path "site-lisp/thing-edit/")

(use-package openwith
  :hook
  (after-init-hook . openwith-mode)
  :config
  ;; (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" " okular" (file))))
  )

;; 自动保存文件
(use-package auto-save
  :load-path "site-lisp/auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-idle 2)
  )

;; 检测文件是否已经被导出 hugo
(defun lxs-org-is-hugo-file-p (fPath)
  "Predict if the org file has been converted into hugo"
  (with-temp-buffer
    (let ((keyline "#+HUGO_DRAFT: false\n"))
	  (insert-file-contents fPath)
	  (and (search-forward keyline nil t) t)
	  )
    ))
(defun lxs-list-org-in-directory (dPath)
  "list org files under a directory path"
  (directory-files-recursively dPath "\.org$")
  )
(defun my--export-to-hugo (dPath)
  "Convert org files under a directory path into hugo .md files"
  (mapc
   (lambda (file-name)
     (progn
       (if (lxs-org-is-hugo-file-p file-name)		
	   (with-current-buffer (find-file-noselect file-name)
	     (org-hugo-export-wim-to-md)
	     ))
       ))
   (lxs-list-org-in-directory dPath)
   )
  (message "export to hugo md file end!"))
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

(use-package keyfreq
  :hook
  (after-init . keyfreq-mode)
  (after-init . keyfreq-autosave-mode)
  :config
  (setq keyfreq-excluded-commands
      '(self-insert-command
        forward-char
        backward-char
        previous-line
        next-line
	hydra-reading/next-line
	hydra-reading/previous-line
	hydra-reading/forward-char
	hydra-reading/backward-char
	hydra-reading/body
	hydra-reading/nil
	org-self-insert-command
	mwheel-scroll
	mouse-set-point
	mouse-drag-region
	iscroll-next-line
	iscroll-previous-line
	ivy-previous-line
	ivy-next-line
	helm-next-line
	helm-previous-line
	)))

(use-package awesome-tab
  :load-path "site-lisp/awesome-tab"
  :config
  (setq awesome-tab-height 120)
  (awesome-tab-mode))

;; (setq global-visual-line-mode t)
;; (setq truncate-lines nil)

(provide 'init-basic)
