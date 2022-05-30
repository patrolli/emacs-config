  ;; Save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  ;; (setq desktop-globals-to-save
  ;;       '((comint-input-ring        . 50)
  ;;         (compile-history          . 30)
  ;;         desktop-missing-file-warning
  ;;         (dired-regexp-history     . 20)
  ;;         (extended-command-history . 30)
  ;;         (face-name-history        . 20)
  ;;         (file-name-history        . 100)
  ;;         (grep-find-history        . 30)
  ;;         (grep-history             . 30)
  ;;         (magit-revision-history   . 50)
  ;;         (minibuffer-history       . 50)
  ;;         (org-clock-history        . 50)
  ;;         (org-refile-history       . 50)
  ;;         (org-tags-history         . 50)
  ;;         (query-replace-history    . 60)
  ;;         (read-expression-history  . 60)
  ;;         (regexp-history           . 60)
  ;;         (regexp-search-ring       . 20)
  ;;         register-alist
  ;;         (search-ring              . 20)
  ;;         (shell-command-history    . 50)
  ;;         tags-file-name
  ;;         tags-table-list))

 (use-package desktop
   :init (desktop-save-mode)
   :config
   ;; inhibit no-loaded prompt
   (setq desktop-file-modtime (file-attribute-modification-time
                               (file-attributes
				(desktop-full-file-name)))
         desktop-lazy-verbose nil
         desktop-load-locked-desktop t
         desktop-restore-eager 1
         desktop-restore-frames nil
         desktop-save t
	 desktop-path (list user-emacs-directory)
	 desktop-auto-save-timeout 120))

(provide 'init-restore)
