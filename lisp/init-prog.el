(use-package lsp-bridge
  :load-path "site-lisp/lsp-bridge"
  :config
  (setq lsp-bridge-completion-provider 'corfu)
  (require 'corfu)
  (require 'corfu-info)
  (require 'corfu-history)
  (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
  (corfu-history-mode t)
  (global-lsp-bridge-mode)
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3)))))))

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
  ;; :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (pyvenv-workon "py39")
  )

(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
