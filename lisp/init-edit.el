(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure t 
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (add-function :before-until electric-pair-inhibit-predicate
		(lambda (c) (eq c ?<))))

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

;; (use-package imenu
;;   :ensure t
;;   :bind
;;   (("C-'" . imenu-list-smart-toggle))
;;   :config
;;   (setq imenu-list-position 'right)
;;   (setq imenu-list-focus-after-activation t)
;;   )

(use-package rainbow-mode
  :ensure t
  :config
  (defun @-enable-rainbow ()
    (rainbow-mode t))
  (add-hook 'lisp-interaction-mode-hook '@-enable-rainbow))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (defun @-enable-rainbow-delimiters ()
      (rainbow-delimiters-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow-delimiters)
    (add-hook 'lisp-interaction-mode-hook '@-enable-rainbow-delimiters)))

;; Nice writing
;TODO: 有延迟加载的问题
(use-package olivetti
  :diminish
  :bind ("<f8>" . olivetti-mode)
  :init
  (setq olivetti-body-width 0.618)
  (defun xs-toggle-olivetti-for-org ()
    "if current buffer is org and only one visible buffer
  enable olivetti mode"
    (if (and (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
	     (or (eq (length (window-list nil nil nil)) 1)
		 (window-at-side-p (frame-first-window) 'right))) ;; frame-first-window 的 mode 是 org-mode 并且没有右边 window
	(olivetti-mode 1)
      (olivetti-mode 0)
      (when (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
	(visual-line-mode 1))))
  (add-hook 'org-mode-hook #'xs-toggle-olivetti-for-org)
  (add-hook 'window-configuration-change-hook #'xs-toggle-olivetti-for-org))

;; jump to things in Emacs tree-style
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

;; display ^L as a line to split file into blocks
;; navigated by C-x [ and C-x ]
(use-package page-break-lines
  :init
  (global-page-break-lines-mode))

;; jump between marker's position
(use-package backward-forward
  :config
  (backward-forward-mode))

(provide 'init-edit)
