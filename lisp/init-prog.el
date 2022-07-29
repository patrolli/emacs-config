(use-package lsp-bridge
  :load-path "site-lisp/lsp-bridge"
  ;; :disabled t
  :config
  (define-key prog-mode-map "\M-." #'lsp-bridge-find-def)
  (define-key prog-mode-map "\M-," #'lsp-bridge-return-from-def)
  (setq lsp-bridge-enable-diagnostics nil)
  (add-hook 'python-mode-hook #'lsp-bridge-mode)
  (define-key acm-mode-map "\M-j" #'(lambda () (interactive) (acm-hide) (hydra-reading/body)))
  (define-key acm-mode-map "\M-k" #'(lambda () (interactive) (acm-hide) (pyim-convert-string-at-point)))

  (defvar xs-lsp-bridge-mode-enabled t)
  (defun toggle-lsp-bridge-mode ()
    (interactive)
    (if xs-lsp-bridge-mode-enabled
	(progn
	  (call-interactively #'lsp-bridge-kill-process)
	  (setq lsp-bridge-mode nil)
	  (remove-hook 'python-mode-hook #'lsp-bridge-mode)
	  (setq xs-lsp-bridge-mode-enabled nil)
	  (global-company-mode t)
	  (define-key prog-mode-map "\M-." #'xref-find-definitions)
	  (define-key prog-mode-map "\M-," #'xref-pop-marker-stack)
	  (message "lsp-bridge closed")
	  )
      (global-company-mode nil)
      (add-hook 'python-mode-hook #'lsp-bridge-mode)
      (setq xs-lsp-bridge-mode-enabled t)
      (define-key prog-mode-map "\M-." #'lsp-bridge-find-def)
      (define-key prog-mode-map "\M-," #'lsp-bridge-return-from-def)
      (message "lsp-bridge enabled")))
  )

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (setq read-process-output-max (* 1024 1024))
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  )

;; python
(use-package pyvenv
  :ensure t
  :disabled t
  :hook (python-mode . pyvenv-mode)
  ;; :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (pyvenv-workon "py39")
  )

(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
