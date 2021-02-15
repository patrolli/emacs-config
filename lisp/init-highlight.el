;; highlight todo related keywords 
(use-package hl-todo
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode)
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")
	("NOTE" . "#b1951d")))
  (define-key hl-todo-mode-map (kbd "C-c b") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c f") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  (diff-hl-insert ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

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

(provide 'init-highlight)
