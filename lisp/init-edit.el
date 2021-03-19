(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (add-function :before-until electric-pair-inhibit-predicate
		(lambda (c) (eq c ?<))))

;; expand-region 选择一块区域
(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region)
  ("C--" . 'er/contract-region))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)

  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

(use-package imenu
  :ensure nil
  :bind
  (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-position 'right)
  (setq imenu-list-focus-after-activation t)   
  )

(use-package rainbow-mode
  :ensure t
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
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

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f8>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-." . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

(provide 'init-edit)
