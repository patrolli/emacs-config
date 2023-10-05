(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (setq read-process-output-max (* 1024 1024))
  (setq eldoc-echo-area-use-multiline-p nil))

;; python
(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package conda
  :ensure t)

(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
