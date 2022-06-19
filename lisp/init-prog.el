;; (use-package lsp-bridge
;;   :load-path "site-lisp/lsp-bridge"
;;   :config
;;   (global-lsp-bridge-mode)
;;   )
(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)


(use-package lsp-bridge-icon
  :load-path "site-lisp/lsp-bridge")

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
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (pyvenv-workon "py39"))

(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
