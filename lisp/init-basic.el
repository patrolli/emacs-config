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
  (after-init . openwith-mode)
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
  (awesome-tab-mode)
  (defun awesome-tab-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       ;; Hide tab if current window is not dedicated window.
       (window-dedicated-p (selected-window))
       ;; Hide sdcv tab.
       (string-prefix-p "*sdcv" name)
       ;; Hide tab if current buffer is helm buffer.
       (string-prefix-p "*helm" name)
       ;; Hide tab if current buffer is flycheck buffer.
       (string-prefix-p "*flycheck" name)
       ;; Hide blacklist if emacs version < 27 (use header-line).
       (and (eq awesome-tab-display-line 'header-line)
            (or (string-prefix-p "*Compile-Log*" name)
		(string-prefix-p "*Flycheck" name)))
       ;; my custom hide rules
       (string-suffix-p ".org_archive" name)
       )))
  (defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((derived-mode-p 'vterm-mode) ;; vterm 的 buffer 带有了 * 号，所以需要放到 Emacs group 的前面
     "Term")
    ((string-match-p (regexp-quote "*elfeed") (buffer-name))
     "Elfeed")
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    ((derived-mode-p 'eaf-mode)
     "EAF")
    (t
     (awesome-tab-get-group-name (current-buffer))))))
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
;; (setq global-visual-line-mode t)
;; (setq truncate-lines nil)

(provide 'init-basic)
