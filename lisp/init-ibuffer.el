(require 'init-funcs)

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Display icons for buffers
  (use-package all-the-icons-ibuffer
    :init (all-the-icons-ibuffer-mode 1))

  (with-eval-after-load 'counsel
    (with-no-warnings
      (defun my-ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))))



;; (add-hook 'ibuffer-hook
;;     (lambda ()
;;       (ibuffer-vc-set-filter-groups-by-vc-root)
;;       (unless (eq ibuffer-sorting-mode 'alphabetic)
;;         (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-saved-filter-groups
  '(("home"
     ("emacs-config" (or (filename . ".emacs.d")
                         (filename . ".emacs")))         
     ("Org" (or (mode . org-mode)
                (filename . ".org")))
     ("Help" (or (name . "\*Help\*")
                 (name . "\*Apropos\*")
                 (name . "\*info\*")))
     ("Vterm" (or (mode . vterm-mode))))))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))

(provide 'init-ibuffer)
