 (when (>= emacs-major-version 24)   
   (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

 ;; cl - Common Lisp Extension
 (require 'cl)

 ;; Add Packages
 (defvar my/packages '(
		;; --- Auto-completion ---
		company
		company-lsp
		;; --- Better Editor ---
		hungry-delete
		smex
		swiper
		counsel
		smartparens
		popwin
		pyim
		super-save
		cnfonts
		neotree
		iedit
		expand-region
		lsp-mode
		flycheck
		use-package
		org-ref
		helm-org-rifle
		magit
		;; --- Major Mode ---
		js2-mode
		xahk-mode
		;; --- Minor Mode ---
		nodejs-repl
		exec-path-from-shell
		cdlatex
		undo-tree
		ace-jump-mode
		window-numbering
		openwith
		imenu-list
		ox-wk
		yasnippet
		;; --- Themes ---
		monokai-theme
		solarized-theme
		leuven-theme
		material-theme
		;; solarized-theme
		) "Default packages")

 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

(require 'pyim)
(setq default-input-method "pyim")
(setq pyim-default-scheme 'xiaohe-shuangpin)
(setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
(setq pyim-dcache-backend 'pyim-dregcache)
;; 自动半角全角切换
(setq-default pyim-punctuation-half-width-functions
	      '(pyim-probe-punctuation-line-beginning
		pyim-probe-punctuation-after-punctuation))
;;(setq pyim-punctuation-translate-p '(no yes auto))

;; 添加词库
(setq pyim-dicts
      '((:name "pyim-bigdict" :file "/mnt/c/Users/lixun/Downloads/pyim-bigdict.pyim")))
(add-hook 'emacs-startup-hook
          #'(lambda () (pyim-restart-1 t)))

(setq pyim-translate-trigger-char "@")
(setq pyim-page-tooltip 'posframe)
;; 使 ivy 支持拼音搜索
(defun eh-ivy-cregexp (str)
    (let ((a (ivy--regex-plus str))
          (b (let ((case-fold-search nil))
               (pyim-cregexp-build str))))
      (if (and a (stringp a))
          (concat a "\\|" b)
        a)))
(setq ivy-re-builders-alist
        '((t . eh-ivy-cregexp)))

;; open global Company
(setq company-global-modes '(not org-mode));; 希望关闭 company 在 orgmode 中的补全
(global-company-mode 1)
(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (setq company-idle-delay 0)
  (setq company-show-numbers t))

;; hungry-delete setting
(require 'hungry-delete)
;; (global-hungry-delete-mode)
(add-hook 'prog-mode-hook #'hungry-delete-mode)
;; config for swiper
(ivy-mode 1)
(setq ivy-use-vitual-buffers t)

;;smartparens
(require 'smartparens-config)
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
;; (smartparens-global-mode t)
;; 使用 emacs 自带的 electric-pair-mode 取代 smartparens
(electric-pair-mode t)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-function :before-until electric-pair-inhibit-predicate
  (lambda (c) (eq c ?<)))

;; config for popwin
(require 'popwin)
(popwin-mode 1)


;; cnfonts
;;(require 'cnfonts)
;;(cnfonts-enable)
;;(cnfonts-set-spacemacs-fallback-fonts)
;; cnfonts 用来设置字体大小
;; 目前较为舒服的设置是，英文字号 12.5
(use-package cnfonts
  :ensure t
  :defer t
  :init
  (cnfonts-enable)
  :config
  (cnfonts-set-spacemacs-fallback-fonts)
  )
;; iedit 可以同时编辑多块区域
(use-package iedit
  :ensure t
  :bind
  ("M-s e" . 'iedit-mode))

;; expand-region 选择一块区域
(require 'expand-region)

;; 自动保存文件
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save


;;
;;(setq lsp-keymap-prefix "s-l")
;;(require 'lsp-mode)
;; Enable LSP backend.
;;(push 'company-lsp company-backends)
;;(add-hook 'python-mode-hook #'lsp)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "s-l")

;; (use-package lsp-mode
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;; 	 (c-mode . lsp)
;; 	 (c++-mode .  lsp)
;; 	 (python-mode . lsp))
;;   :commands lsp)
;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
;; (setq lsp-diagnostic-package :none)
;; (setq lsp-diagnostic-package :none)
;; (require 'lsp-python-ms)
;; (setq lsp-python-ms-auto-install-server t)
;; (setq lsp-python-ms-executable
;;       "~/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")

(use-package valign
        :load-path "~/.emacs.d/valign/"
        :config
        (add-hook 'org-mode-hook #'valign-mode))

;; flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode -1)

;; 切换窗口
(window-numbering-mode 1)

;; undo-tree, C-x u: show the undo-tree for current file
(require 'undo-tree)
(global-undo-tree-mode 1)

;; i want to open pdf with external app
(require 'openwith)
(openwith-mode t)
;; (setq openwith-associations '(("\\.pdf\\'" "qpdfview" (file))))
;; (setq openwith-associations '(("\\.pdf\\'" " ~/bin/FoxitReader" (file))))
(setq openwith-associations '(("\\.pdf\\'" " okular" (file))))

;; imenu-list
;; (global-set-key (kbd "C-c l") 'imenu-list-minor-mode)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(setq imenu-list-position 'right)
(setq imenu-list-focus-after-activation t)

;; wiki format converter from org
(require 'ox-wk)
;; yasnippet, code template
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)

;; projectile configuration
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config
  (require 'projectile))

(use-package rainbow-mode
  :ensure t
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow)
    (add-hook 'lisp-interaction-mode-hook '@-enable-rainbow)
    ))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (defun @-enable-rainbow-delimiters ()
      (rainbow-delimiters-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow-delimiters)
    (add-hook 'lisp-interaction-mode-hook '@-enable-rainbow-delimiters)))

(use-package bm
         :ensure t
         :demand t

         :init
         ;; restore on load (even before you require bm)
         (setq bm-restore-repository-on-load t)


         :config
         ;; Allow cross-buffer 'next'
         (setq bm-cycle-all-buffers t)

         ;; where to store persistant files
         (setq bm-repository-file "~/.emacs.d/bm-repository")

         ;; save bookmarks
         (setq-default bm-buffer-persistence t)

         ;; Loading the repository from file when on start up.
         (add-hook 'after-init-hook 'bm-repository-load)

         ;; Saving bookmarks
         (add-hook 'kill-buffer-hook #'bm-buffer-save)

         ;; Saving the repository to file when on exit.
         ;; kill-buffer-hook is not called when Emacs is killed, so we
         ;; must save all bookmarks first.
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))

         ;; The `after-save-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state.
         (add-hook 'after-save-hook #'bm-buffer-save)

         ;; Restoring bookmarks
         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)

         ;; The `after-revert-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state. This hook might cause trouble when using packages
         ;; that automatically reverts the buffer (like vc after a check-in).
         ;; This can easily be avoided if the package provides a hook that is
         ;; called before the buffer is reverted (like `vc-before-checkin-hook').
         ;; Then new bookmarks can be saved before the buffer is reverted.
         ;; Make sure bookmarks is saved before check-in (and revert-buffer)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


         :bind (("<f3>" . bm-next)
                ("S-<f3>" . bm-previous)
                ("C-<f3>" . bm-toggle))
         )

(defun bm-counsel-get-list (bookmark-overlays)
  (-map (lambda (bm)
          (with-current-buffer (overlay-buffer bm)
            (let* ((line (replace-regexp-in-string "\n$" "" (buffer-substring (overlay-start bm)
                                                                              (overlay-end bm))))
                   ;; line numbers start on 1
                   (line-num (+ 1 (count-lines (point-min) (overlay-start bm))))
                   (name (format "%s:%d - %s" (buffer-name) line-num line)))

              `(,name . ,bm))))
        bookmark-overlays))

(defun counsel-bm-update-input ()
  "Update fn for counsel-bm."
  (with-ivy-window
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((chosen (ivy-state-current ivy-last))
             (bookmark (gethash chosen bm-hash-table)))
        (if chosen
            (save-restriction
              (with-ivy-window
                (switch-to-buffer (overlay-buffer bookmark))
                (bm-goto bookmark)))
          nil)))))

(defun counsel-bm (&optional initial-input)
  "Use ivy to select bm bookmarks.
It has the ability to preview the bookmarks like `swiper-all'."
  (interactive)
  (let* ((bm-list (bm-counsel-get-list (bm-overlays-lifo-order t)))
         (bm-hash-table (make-hash-table :test 'equal))
         (search-list (-map (lambda (bm) (car bm)) bm-list)))

    (-each bm-list (lambda (bm)
                     (puthash (car bm) (cdr bm) bm-hash-table)))

    (if search-list
        (ivy-read "Find bookmark: "
                  search-list
                  :keymap counsel-describe-map

                  :action (lambda (chosen)
                            (let ((bookmark (gethash chosen bm-hash-table)))
                              (switch-to-buffer (overlay-buffer bookmark))
                              (bm-goto bookmark)))

                  :update-fn #'counsel-bm-update-input

                  :initial-input initial-input
                  :caller 'counsel-bm
                  )
      (message "%s" "No bookmark now."))))



(defun counsel-bm-from-isearch ()
  "Invoke `counsel-bmr' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (counsel-bm query)))

(define-key isearch-mode-map (kbd "<M-return>") 'counsel-bm-from-isearch)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

(use-package calibredb
  :ensure t
  ;; :defer t
  :init
  ;; (autoload 'calibredb "calibredb")
  :config
  (setq calibredb-root-dir "/mnt/c/Users/lixun/Documents/Calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("/mnt/c/Users/lixun/Documents/Calibre")))
  (setq sql-sqlite-program "/usr/bin/sqlite3")
  (setq calibredb-program "/usr/bin/calibredb")
  (setq calibredb-format-all-the-icons t)
  (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  ;; (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography)   ;; 这里要先加载 org-ref
  (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)
  )

(use-package hl-todo
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode)
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))
  (define-key hl-todo-mode-map (kbd "C-c b") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c f") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert))

(use-package json-mode
  :ensure t)

(use-package ebib
  :ensure t
  :config
  (setq ebib-preload-bib-files ["/mnt/c/Users/lixun/Documents/bibliography/library.bib"]))

;;pdf tools
(use-package pdf-tools
   :pin manual
   :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-width)
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)

(use-package helm-org
  :ensure t)

(use-package go-translate
  :ensure t
  :config
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-token-current (cons 430675 2721866130))
  (global-set-key "\C-ct" 'go-translate)
  (setq go-translate-buffer-follow-p t)  
  )

(use-package posframe
  :ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-common-docsets '("Python 3" "PyTorch"))
  (setq helm-dash-browser-func 'eww)  
)

(use-package vterm
  :ensure t
  :config
  (setq vterm-buffer-name-string nil))

(use-package centaur-tabs
  :demand
  :ensure t
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-height 22)
  (setq centaur-tabs-close-button "x")
  (setq centaur-tabs-cycle-scope 'tabs)
   (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
	(cond
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
	 ((derived-mode-p 'prog-mode)
	  "Editing")
	 ((derived-mode-p 'dired-mode)
	  "Dired")
	 ((derived-mode-p 'vterm-mode)
	  "Vterm")
	 ((memq major-mode '(helpful-mode
			     help-mode))
	  "Help")
	 ((memq major-mode '(org-mode
			     org-agenda-clockreport-mode
			     org-src-mode
			     org-agenda-mode
			     org-beamer-mode
			     org-indent-mode
			     org-bullets-mode
			     org-cdlatex-mode
			     org-agenda-log-mode
			     diary-mode))
	  "OrgMode")
	 (t
	  (centaur-tabs-get-group-name (current-buffer))))))
   (defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "*NOX" name)
     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	  (not (file-name-extension name)))
     )))
   (defun tabs-timer-initialize (secs)
  (setq tabs-timer (run-with-timer secs nil (lambda () (centaur-tabs-local-mode 1)))))

(defun tabs-timer-hide ()
  (tabs-timer-initialize 3))

;; (add-hook 'window-setup-hook 'tabs-timer-hide)
;; (add-hook 'find-file-hook 'tabs-timer-hide)

(defun centaur-tabs-switch-and-hide (arg)
  (cancel-timer tabs-timer)
  (centaur-tabs-local-mode 1)
  (cond ((equal arg 'backward)
         (centaur-tabs-backward))
        ((equal arg 'forward)
         (centaur-tabs-forward))
        ((equal arg 'backward-group)
         (centaur-tabs-backward-group))
        ((equal arg 'forward-group)
         (centaur-tabs-forward-group)))
  (centaur-tabs-local-mode 0)
  (setq tabs-timer (tabs-timer-initialize 2)))

(defun centaur-tabs-forward-and-hide ()
  (interactive)
  (centaur-tabs-switch-and-hide 'forward))

(defun centaur-tabs-backward-and-hide ()
  (interactive)
  (centaur-tabs-switch-and-hide 'backward))

(defun centaur-tabs-forward-group-and-hide ()
  (interactive)
  (centaur-tabs-switch-and-hide 'forward-group))

(defun centaur-tabs-backward-group-and-hide ()
  (interactive)
  (centaur-tabs-switch-and-hide 'backward-group))
  :bind
  ;; ("C-x ," . centaur-tabs-backward-and-hide)
  ;; ("C-x /" . centaur-tabs-forward-and-hide)
  ("C-x t s" . centaur-tabs-counsel-switch-group)
  ("C-x t p" . centaur-tabs-group-by-projectile-project)
  ("C-x t g" . centaur-tabs-group-buffer-groups))



(provide 'init-packages)


