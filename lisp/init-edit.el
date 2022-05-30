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

;; ;; expand-region
(use-package expand-region)

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
  :ensure t
  :bind
  (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-position 'right)
  (setq imenu-list-focus-after-activation t)
  )

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

;; Flexible text folding
(use-package origami
  :pretty-hydra
  ((:title (pretty-hydra-title "Origami" 'octicon "fold" :height 1.1 :v-adjust -0.05)
    :color amaranth :quit-key "q")
   ("Node"
    ((":" origami-recursively-toggle-node "toggle recursively")
     ("a" origami-toggle-all-nodes "toggle all")
     ("t" origami-toggle-node "toggle current")
     ("o" origami-show-only-node "only show current"))
    "Actions"
    (("u" origami-undo "undo")
     ("d" origami-redo "redo")
     ("r" origami-reset "reset"))))
  :bind (:map origami-mode-map
         ("C-`" . origami-hydra/body))
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))

;; display ^L as a line to split file into blocks
;; navigated by C-x [ and C-x ]
(use-package page-break-lines
  :init
  (global-page-break-lines-mode)
  )


;; auto-insert file headers
;; https://honmaple.me/articles/2018/01/emacs%E8%87%AA%E5%8A%A8%E6%B7%BB%E5%8A%A0%E6%96%87%E4%BB%B6%E5%A4%B4.html
(defun maple--insert-string()
  (concat
   (make-string 80 ?*)
   "\n"
   "Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
   "File Name: " (file-name-nondirectory buffer-file-name) "\n"
   "Author: " (user-full-name)"\n"
   "Email: " user-mail-address "\n"
   "Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
   "Last Update: \n"
   "         By: \n"
   "Description: \n"
   (make-string 80 ?*)))

(defun maple-insert-string(&optional prefix)
  (replace-regexp-in-string
   "^" (or prefix comment-start)
   (maple--insert-string)))

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n"
         "# -*- encoding: utf-8 -*-\n"
         (maple-insert-string) "\n")
        ((python-mode . "Python program") nil
         ;; "#!/usr/bin/env python\n"
         ;; "# -*- coding: utf-8 -*-\n"
         (maple-insert-string) "\n")
        ((c-mode . "C program") nil
         "/*"
         (string-trim-left (maple-insert-string " ")) "*/\n"
         "#include<stdio.h>\n"
         "#include<string.h>\n")
        ((sh-mode . "Shell script") nil
         "#!/bin/bash\n"
         (maple-insert-string) "\n")
        ((go-mode . "Go program") nil
         "/*"
         (string-trim-left (maple-insert-string " ")) "*/\n")))

;; https://honmaple.me/articles/2018/01/emacs%E8%87%AA%E5%8A%A8%E6%9B%B4%E6%96%B0%E6%96%87%E4%BB%B6%E5%A4%B4.html
(setq time-stamp-active t)
(setq time-stamp-line-limit 11)
(setq time-stamp-start "[lL]ast[ -][uU]pdate[ \t]*:?")
(setq time-stamp-end "\n")
(setq time-stamp-format " %#A %Y-%02m-%02d %02H:%02M:%02S (%Z)")
(add-hook 'before-save-hook 'time-stamp)

(defun maple/header-update-action(name)
  "A."
  (let ((beg (match-beginning 2))
        (end (match-end 2)))
    (when (not (string= name (string-trim-left (match-string 2))))
      (goto-char beg)
      (delete-region beg end)
      (insert " " name))))

(defun maple/header-update(regex default line-limit)
  "B."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((lines 0))
      (while (< lines line-limit)
        (when (and (looking-at regex))
          (maple/header-update-action default))
        (setq lines (1+ lines))
        (forward-line 1)))))
(defmacro maple/header-update-engine (name regex default &optional line-limit)
  "C."
  `(defun ,(intern (format "maple/header-update-%s" name)) ()
     ,(format "Update %s with regex." name)
     (interactive)
     (maple/header-update ,regex ,default ,(or line-limit 7))))

(maple/header-update-engine "filename"
                            ".*\\(File Name:\\)\\(.*\\)"
                            (file-name-nondirectory (buffer-file-name)) 7)

(maple/header-update-engine "email"
                            ".*\\(Email:\\)\\(.*\\)"
                            "youemail@gmail.com" 7)
(add-hook 'before-save-hook 'maple/header-update-filename)

;; jump between marker's position
(use-package backward-forward
  :config
  (backward-forward-mode))

(provide 'init-edit)
