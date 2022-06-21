(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
	 ([f4] . vterm-toggle)
	 ("M-j" . hydra-reading/body))
  :config
  (setq vterm-buffer-name-string nil)
  (defun my/vterm ()
  "Open a vterm by selecting a profile from a profile lists.
"
  (interactive)
   (let ((completion-ignore-case  t)
     (command (completing-read "Choose: " '("local"
                                         "aimax-ht -p 25997"; your profiles
                                         "aimax-ht -p 25532"
                                         "aimax-ht -p 25778"
					 "diggers3") nil t)))
  (if
      (not (string-equal command "local"))
      (with-current-buffer (vterm (concat "*" command "*"))
        (vterm-send-string (format "ssh %s" command))
         (vterm-send-return))
    (vterm (concat "*vterm-" command "*"))))))

  (use-package vterm-toggle
  :bind
  ([f4] . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                ;;(direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))
					;Switch to next vterm buffer
  (define-key vterm-mode-map (kbd "M-n")   'vterm-toggle-forward)
					;Switch to previous vterm buffer
  (define-key vterm-mode-map (kbd "M-p")   'vterm-toggle-backward))
)

(provide 'init-term)
