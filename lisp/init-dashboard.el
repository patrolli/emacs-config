(require 'init-funcs)

;; Dashboard
;; (use-package dashboard
;;   :ensure t
;;   :bind (("<f2>" . open-dashboard)
;;          :map dashboard-mode-map
;;          ("H" . browse-homepage)
;;          ("R" . restore-previous-session)
;;          ("L" . restore-session)
;;          ("S" . open-custom-file)
;;          ("q" . quit-dashboard))
;;   :custom
;;   ((dashboard-set-heading-icons t)
;;    (dashboard-set-navigator t)
;;    (dashboard-center-content t))
;;   :init  
;;   (setq dashboard-startup-banner (or lxs-logo 'official)
;; 	dashboard-show-shortcuts nil
;; 	dashboard-set-navigator t
;; 	dashboard-navigator-buttons
;; 	`(((,(when (icons-displayable-p)
;;                  (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
;;               "Homepage" "Browse homepage"
;;               (lambda (&rest _) (browse-url lxs-homepage)))
;;              (,(when (icons-displayable-p)
;;                  (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
;;               "Restore" "Restore previous session"
;;               (lambda (&rest _) (restore-previous-session)))
;;              (,(when (icons-displayable-p)
;;                  (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
;;               "Settings" "Open custom file"
;;               (lambda (&rest _) (find-file custom-file)))  
;;              )))
;;   (dashboard-setup-startup-hook)
;;   :config
;;   ;; (setq dashboard-icon-type 'all-the-icons)
;;   (dashboard-modify-heading-icons '((recents . "file-text")
;;                                     (bookmarks . "book")))

;;   (setq dashboard-projects-backend 'projectile)
;;   (setq dashboard-items '((recents  . 10)
;;                         (bookmarks . 5)
;;                         (projects . 5)))

;;   (defvar dashboard-recover-layout-p nil
;;     "Wether recovers the layout.")

;;   (defun restore-previous-session ()
;;     (interactive)
;;     (desktop-read)
;;     (session-initialize))

;;   (defun dashboard-goto-recent-files ()
;;     "Go to recent files."
;;     (interactive)
;;     (let ((func (local-key-binding "r")))
;;       (and func (funcall func))))

;;   (defun dashboard-goto-projects ()
;;     "Go to projects."
;;     (interactive)
;;     (let ((func (local-key-binding "p")))
;;       (and func (funcall func))))

;;   (defun dashboard-goto-bookmarks ()
;;     "Go to bookmarks."
;;     (interactive)
;;     (let ((func (local-key-binding "m")))
;;       (and func (funcall func))))
  
;;   (defun open-dashboard ()
;;       "Open the *dashboard* buffer and jump to the first widget."
;;       (interactive)
;;       ;; Check if need to recover layout
;;       (if (> (length (window-list-1))
;;              ;; exclude `treemacs' window
;;              (if (and (fboundp 'treemacs-current-visibility)
;;                       (eq (treemacs-current-visibility) 'visible))
;;                  2
;;                1))
;;           (setq dashboard-recover-layout-p t))

;;       (delete-other-windows)

;;       ;; Refresh dashboard buffer
;;       (when (get-buffer dashboard-buffer-name)
;;         (kill-buffer dashboard-buffer-name))
;;       (dashboard-insert-startupify-lists)
;;       (switch-to-buffer dashboard-buffer-name)

;;       ;; Jump to the first section
;;       (dashboard-goto-recent-files))
  
;;   (defun quit-dashboard ()
;;       "Quit dashboard window."
;;       (interactive)
;;       (quit-window t)
;;       (when (and dashboard-recover-layout-p
;;                  (bound-and-true-p winner-mode))
;;         (winner-undo)
;;         (setq dashboard-recover-layout-p nil))))

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
    (setq dashboard-startup-banner (or lxs-logo 'official))
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)))
  )

(provide 'init-dashboard)
