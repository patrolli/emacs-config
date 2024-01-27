(require 'init-funcs)

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "Welcom, emacs loves you")
    (setq dashboard-startup-banner (or lxs-logo 'official))
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)))
  )

(provide 'init-dashboard)
