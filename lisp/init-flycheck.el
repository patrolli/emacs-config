(require 'init-const)
(require 'init-funcs)
(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-global-modes
              '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
              ;; Only check while saving and opening files
              flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (setq flycheck-flake8rc "~/.flake8")
  (setq flycheck-python-flake8-executable "python")
  )
(provide 'init-flycheck)
