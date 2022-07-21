(use-package lsp-bridge
  :load-path "site-lisp/lsp-bridge"
  ;; :disabled t
  :config
  (define-key prog-mode-map "\M-." #'lsp-bridge-find-def)
  (define-key prog-mode-map "\M-," #'lsp-bridge-return-from-def)
  (setq lsp-bridge-enable-diagnostics nil)
  (add-hook 'python-mode-hook #'lsp-bridge-mode)
  (define-key acm-mode-map "\M-j" #'(lambda () (interactive) (acm-hide) (hydra-reading/body)))
  (define-key acm-mode-map "\M-k" #'(lambda () (interactive) (acm-hide) (pyim-convert-string-at-point))))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (setq read-process-output-max (* 1024 1024))
  (setq eldoc-echo-area-use-multiline-p nil))

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
