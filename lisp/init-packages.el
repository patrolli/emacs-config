 
;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像
;;
 ;; cl - Common Lisp Extension
 (require 'cl-lib)

(use-package pyim
  :ensure nil
  :disabled t
  :demand t
  :init
  ;; (setq pyim-dicts
  ;; 	'((:name "pyim-bigdict" :file "/mnt/c/Users/lixun/Downloads/pyim-bigdict.pyim")))  ;; 添加词库
  (setq pyim-dicts
	'((:name "pyim-another-dict" :file "~/.emacs.d/pyim-dict/pyim-another-dict.pyim")))  ;; 添加词库
  (setq pyim-translate-trigger-char "@")
  (setq pyim-page-length 5)
  (setq pyim-page-tooltip 'posframe)
  (setq pyim-default-scheme 'xiaohe-shuangpin)
  :hook
  ('emacs-startup-hook . (lambda () (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer))
  :config
  ;; (use-package pyim-basedict
  ;;   :ensure t
  ;;   :config (pyim-basedict-enable))
  (setq default-input-method "pyim")
  (setq pyim-dcache-backend 'pyim-dregcache)
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
	      '(pyim-probe-punctuation-line-beginning
		pyim-probe-punctuation-after-punctuation)) ;; 自动半角全角切换
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
  )

(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq default-input-method "rime"
	rime-show-candidate 'posframe);;
  (setq rime-user-data-dir "~/.config/fcitx/rime")
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-space-after-cc-p
          rime-predicate-punctuation-after-space-cc-p
          rime-predicate-prog-in-code-p
	  ;; rime-predicate-current-input-punctuation-p
          ;; rime-predicate-after-ascii-char-p
	  rime-predicate-punctuation-after-ascii-p
	  ;; rime-predicate-space-after-ascii-p
          rime-predicate-current-uppercase-letter-p
          ))
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))
  (add-hook 'find-file-hook #'toggle-input-method) 
  )


;; open global Company
(setq company-global-modes '(not org-mode));; 希望关闭 company 在 orgmode 中的补全
(global-company-mode 1)

(use-package company-tabnine
  :defer 1
  :disabled t  ;; 停用 tabnine
  :custom
  (company-tabnine-max-num-results 9)
  :bind
  (("M-q" . company-other-backend))
  :hook
  (lsp-after-open . (lambda ()
                      (setq company-tabnine-max-num-results 3)
                      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                      (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))))
  (kill-emacs . company-tabnine-kill-process)
  :config
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)
  (setq company-idle-delay 0.2)
  (setq company-show-numbers t)
  (add-hook 'nox-managed-mode-hook
           #'(lambda ()
               (when (nox-managed-p)
                 (setq-local company-backends
                            (append  company-backends '(company-tabnine))))))
  ;; Integrate company-tabnine with lsp-mode
;;   (defun company//sort-by-tabnine (candidates)
;;     (if (or (functionp company-backend)
;;             (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
;;         candidates
;;       (let ((candidates-table (make-hash-table :test #'equal))
;;             candidates-lsp
;;             candidates-tabnine)
;;         (dolist (candidate candidates)
;;           (if (eq (get-text-property 0 'company-backend candidate)
;;                   'company-tabnine)
;;               (unless (gethash candidate candidates-table)
;;                 (push candidate candidates-tabnine))
;;             (push candidate candidates-lsp)
;;             (puthash candidate t candidates-table)))
;;         (setq candidates-lsp (nreverse candidates-lsp))
;;         (setq candidates-tabnine (nreverse candidates-tabnine))
;;         (nconc (seq-take candidates-tabnine 3)
;;                (seq-take candidates-lsp 6)))))
;;   (setq company-tabnine--disable-next-transform nil)
;; (defun my-company--transform-candidates (func &rest args)
;;   (if (not company-tabnine--disable-next-transform)
;;       (apply func args)
;;     (setq company-tabnine--disable-next-transform nil)
;;     (car args)))

;; (defun my-company-tabnine (func &rest args)
;;   (when (eq (car args) 'candidates)
;;     (setq company-tabnine--disable-next-transform t))
;;   (apply func args))

;; (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
;; (advice-add #'company-tabnine :around #'my-company-tabnine)
)

;; config for swiper
(use-package ivy
  :ensure t
  :bind
  (("\C-s" . 'swiper)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-c C-r" . 'ivy-resume)
   ("<f1> f" . 'counsel-describe-function)
   ("<f1> v" . 'counsel-describe-variable)
   ("C-c <f3>" . 'counsel-bm)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c g" . 'counsel-git)
   ("C-c j" . 'counsel-git-grep)
   ("C-c k" . 'counsel-ag))
  (:map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :config
  (ivy-mode 1)
  (setq ivy-wrap t)
  (setq ivy-use-vitual-buffers t))

;;smartparens
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode);
;; 使用 emacs 自带的 electric-pair-mode 取代 smartparens
(electric-pair-mode t)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-function :before-until electric-pair-inhibit-predicate
  (lambda (c) (eq c ?<)))

;; config for popwin
(use-package popwin
  :defer t
  :config
  (popwin-mode 1))

;; cnfonts 用来设置字体大小
;; 目前较为舒服的设置是，英文字号 12.5
;; (use-package cnfonts
;;   :ensure t
;;   :defer t
;;   :init
;;   (cnfonts-enable)
;;   :config
;;   (cnfonts-set-spacemacs-fallback-fonts)
;;   )

;; expand-region 选择一块区域
(use-package expand-region
  :defer t
  :bind
  ("C-=" . 'er/expand-region)
  ("C--" . 'er/contract-region))


;; 自动保存文件
(use-package auto-save
  :load-path "~/.emacs.d/auto-save/"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-idle 2)
  )

(use-package valign
        :load-path "~/.emacs.d/valign/"
        :config
        (add-hook 'org-mode-hook #'valign-mode))

;; 切换窗口
;; (window-numbering-mode 1)
(use-package winum
  :ensure t
  :defer t
  :hook
  (after-init . winum-mode)
  :init
  (setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))
  :config
  (setq window-numbering-scope            'global
      winum-reverse-frame-list          nil
      winum-auto-assign-0-to-minibuffer nil
      winum-auto-setup-mode-line        t
      winum-format                      " %s "
      winum-mode-line-position          1
      winum-ignored-buffers             '(" *which-key*")
      winum-ignored-buffers-regexp      '(" \\*Treemacs-.*"))
  (winum-mode)
  )

;; undo-tree, C-x u: show the undo-tree for current file
(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode 1)
  )

;; i want to open pdf with external app
(use-package openwith
  :defer t
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" " okular" (file))))
  )

;; imenu-list
;; (global-set-key (kbd "C-c l") 'imenu-list-minor-mode)
(use-package imenu
  :bind
  (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-position 'right)
  (setq imenu-list-focus-after-activation t)   
   )
;; wiki format converter from org
;; (require 'ox-wk)
;; yasnippet, code template
(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)))

;; projectile configuration
;; to remove project managed by projectile, use the command projectile-remove-known-project
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config
  (setq projectile-switch-project-action 'projectile-dired))

(use-package rainbow-mode
  :ensure t
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    ;; (add-hook 'prog-mode-hook '@-enable-rainbow)
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
  :defer t
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
         ;; (add-hook 'after-init-hook 'bm-repository-load)

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
  :defer t
  :disabled t
  :diminish
  :ensure t
  :config
  (which-key-mode)
  )

(use-package calibredb
  :ensure t
  :defer t
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



(use-package json-mode
  :defer t
  :ensure t)

(use-package ebib
  :defer t
  :ensure t
  :config
  (setq ebib-preload-bib-files ["/mnt/c/Users/lixun/Documents/bibliography/library.bib"]))

;;pdf tools
;; (use-package pdf-tools
;;    :pin manual
;;    :config
;;    (pdf-tools-install)
;;    (setq-default pdf-view-display-size 'fit-width)
;;    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;    :custom
;;    (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)

(use-package helm-org
  :ensure t
  :disabled t
  (defun lxs/helm-org-run-marked-heading-id-link ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action
        'my/helm-org-marked-heading-id-link)))

(defun lxs/helm-org-marked-heading-id-link (marker)
     (let* ((victims (with-helm-buffer (helm-marked-candidates)))
            (buffer (marker-buffer marker))
            (filename (buffer-file-name buffer))
            (rfloc (list nil filename nil marker)))
       (when (and (= 1 (length victims))
                  (equal (helm-get-selection) (car victims)))
         ;; No candidates are marked; we are refiling the entry at point
         ;; to the selected heading
         (setq victims (list marker)))
       (when (and victims buffer filename rfloc)
         (cl-loop for victim in victims
                  ;; do (org-with-point-at victim
                  ;;      (org-refile nil nil rfloc))

                  do (with-current-buffer (marker-buffer victim)
         (let ((heading-id (save-excursion (goto-char (marker-position victim))
                                           (org-id-get-create)
                                           ))
               (heading-name
                (save-excursion
                  (goto-char (marker-position victim))
                  (org-entry-get nil "ITEM"))
                )
               )
           (with-helm-current-buffer
             (org-insert-link
              nil (concat "id:" heading-id) heading-name)
             (insert " ")
             )))
   ))))

(add-to-list 'helm-org-headings-actions '("Insert id link(s) C-c h l" . my/helm-org-marked-heading-id-link) t))

(use-package go-translate
  :defer t
  :ensure t
  :bind
  ("C-c t" . go-translate)
  :config
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-buffer-follow-p t)
  )

(use-package posframe
  :defer t
  :ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package vterm
  :ensure t
  :defer t
  :config
  (setq vterm-buffer-name-string nil))

(use-package centaur-tabs
  :demand
  :ensure t
  :init
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-height 22)
  (setq centaur-tabs-close-button "x")
  (setq centaur-tabs-cycle-scope 'tabs)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
   (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
	(cond
	 ((or
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
			     org-beamer-mode
			     org-indent-mode
			     org-bullets-mode
			     org-cdlatex-mode
			     org-agenda-log-mode
			     diary-mode
			     bibtex-mode))
	  "OrgMode")
	 (t
	  (centaur-tabs-get-group-name (current-buffer))))))
   ;;(centaur-tabs-group-by-projectile-project) ;; 按 projectile 来组织
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
     (string-prefix-p "*Org Note" name)
     (string-prefix-p "*calibredb" name)
     (string-prefix-p "*Backtrace" name)
     (string-prefix-p "*Org Agenda" name)
     (string-prefix-p "*Ilist" name)
     (string-prefix-p "*DBLP" name)
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
  ("C-x C-o" . centaur-tabs-counsel-switch-group)
  ("C-x t p" . centaur-tabs-group-by-projectile-project)
  ("C-c <C-right>" . centaur-tabs-forward)
  ("C-c <C-left>" . centaur-tabs-backward)
  ("C-c <C-up>" . centaur-tabs-backward-group)
  ("C-c <C-down>" . centaur-tabs-forward-group)
  ("C-x t g" . centaur-tabs-group-buffer-groups))


(use-package color-rg
  :load-path "~/.emacs.d/color-rg/"
  :init
  (define-key global-map (kbd "C-x l") (make-sparse-keymap))
  :bind
  (("C-x l p" . color-rg-search-project)
   ("C-x l i" . color-rg-search-input)
   ("C-x l s" . color-rg-search-symbol)
   ("C-x l o" . lxs/search-org)
   )
  )


(global-set-key (kbd "C-l") nil)
(use-package avy
  :defer t
  :bind
  (("C-l c" . avy-goto-char-timer)
   ("C-l l" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#f2241f" :weight bold)))))

(provide 'init-packages)


