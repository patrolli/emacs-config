;; nox, a lightweight lsp client
(use-package nox
  :load-path "site-lisp/nox/"
  :disabled t
  :init
  (defvar nox-python-path "/opt/anaconda/bin/python3")

  ;; :custom
  ;;
  :config
  ;; cpp
   (add-to-list 'nox-server-programs '(c++-mode . ("clangd")))
   (add-to-list 'nox-server-programs '(c-mode . ("clangd")))
   ;; (add-to-list 'nox-server-programs '((python-mode) . "pyright"))
   (setq nox-python-server "pyright")
   (define-key nox-mode-map (kbd "C-.")  'nox-show-doc)
   )

;; (use-package nox
  ;; :load-path "site-lisp/nox/")
;; (setq nox-server-programs (remove (nth 0 nox-server-programs) nox-server-programs))

(defun xref--pop-to-location@around (func item &optional action)
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker))
         (file (xref-location-group (xref-item-location item)))
         (directory (projectile-project-root)))
    (if (not (string-match directory file))
        (with-current-buffer buf
          (read-only-mode)))
    (funcall func item action)
    ))

(use-package citre
  :diminish
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :disabled t
  :init
  (defun lxs/generate-ctags-for-python-project ()
    (interactive)
    (shell-command "ctags --languages=python --kinds-all='*' --fields='*' --extras='*' -R"))
  ;; custom citre backends
  (defun company-citre (-command &optional -arg &rest _ignored)
  "Completion backend of for citre.  Execute COMMAND with ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case -command
    (interactive (company-begin-backend 'company-citre))
    (prefix (and (bound-and-true-p citre-mode)
                 (or (citre-get-symbol) 'stop)))
    (meta (citre-get-property 'signature -arg))
    (annotation (citre-capf--get-annotation -arg))
    (candidates (all-completions -arg (citre-capf--get-collection -arg)))
    (ignore-case (not citre-completion-case-sensitive))))
  )


;; lsp config
(use-package lsp-mode
  :hook
  ((prog-mode . (lambda () (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
				  (lsp-deferred))))) ;; lsp-deferred 保证只有文件是见过了，才会启动 lsp server
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB

  ;; 关掉不必要的功能，保证性能
  (setq lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-headerline-breadcrumb-enable nil

        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  
  ;; :config

  ;; (setq lsp-headerline-breadcrumb-enable nil
	;; lsp-on-idle-hook nil)
  )

(use-package lsp-pyright
       :preface
       ;; Use yapf to format
       (defun lsp-pyright-format-buffer ()
         (interactive)
         (when (and (executable-find "yapf") buffer-file-name)
           (call-process "yapf" nil nil nil "-i" buffer-file-name)))
       ;; :hook (python-mode . (lambda ()
       ;;                        (require 'lsp-pyright)
       ;;                        (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
       :init (when (executable-find "python3")
               (setq lsp-pyright-python-executable-cmd "python3")))

;; (setq python-shell-interpreter "/home/lixunsong/anaconda3/bin/python3.8")
;; python
(use-package pyvenv
  :ensure t
  :disabled t
  :hook (python-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" "/opt/anaconda/envs")
  :config
  (pyvenv-workon "py39")
  )

(use-package helm-dash
  :ensure t
  :disabled t
  :bind
  (:map prog-mode-map ("M-[" . helm-dash-at-point))
  :config
  (setq helm-dash-common-docsets '("Python 3" "PyTorch"))
  (setq helm-dash-browser-func 'eww)
  )

;; leetcode
(use-package leetcode
  :defer t
  :config
  (setq leetcode-prefer-language "python3"
	leetcode-save-solutions t
	leetcode-directory (concat lxs-home-dir "leetcode")))

;;
(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
