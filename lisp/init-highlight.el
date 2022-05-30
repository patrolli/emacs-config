;; highlight todo related keywords 
(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-keyword-faces '(("TODO"   . "#FF0000")
			   ("FIXME"  . "#FF0000")
			   ("DEBUG"  . "#A020F0")
			   ("GOTCHA" . "#FF4500")
			   ("STUB"   . "#1E90FF")
			   ("NOTE" . "#b1951d")
			   ("QUES" . "DarkMagenta")))
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode)
  ;; (setq hl-todo-keyword-faces
      ;; '(("TODO"   . "#FF0000")
        ;; ("FIXME"  . "#FF0000")
        ;; ("DEBUG"  . "#A020F0")
        ;; ("GOTCHA" . "#FF4500")
        ;; ("STUB"   . "#1E90FF")
	;; ("NOTE" . "#b1951d")
	;; ("QUES" . "DarkMagenta")))	
  (define-key hl-todo-mode-map (kbd "C-c b") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c f") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)) 

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  ;; (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  ;; (diff-hl-insert ((t (:background nil))))
  ;; (diff-hl-delete ((t (:background nil))))
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  (diff-hl-insert ((t (:inherit diff-added :background nil))))
  (diff-hl-delete ((t (:inherit diff-removed :background nil))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  ;; (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  ;; Reset faces after changing the color theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (custom-set-faces
               `(diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
               '(diff-hl-insert ((t (:inherit diff-added :background nil))))
               '(diff-hl-delete ((t (:inherit diff-removed :background nil)))))))

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector  #b11100000 #b11111100)
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      (setq diff-hl-margin-symbols-alist
            '((insert . " ") (delete . " ") (change . " ")
              (unknown . " ") (ignored . " ")))
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))
			
;; Highlight symbols
(use-package symbol-overlay
  :disabled t
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-i" . symbol-overlay-put)
         ([M-down] . symbol-overlay-jump-next)
         ([M-up] . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-purple bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

;; advanced systax highting
(use-package tree-sitter
  :config
  :hook
  ((python-mode c++-mode c-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(provide 'init-highlight)
