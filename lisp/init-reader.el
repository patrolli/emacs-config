(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(define-minor-mode centaur-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group centaur
  (if centaur-read-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
        (text-scale-set +2))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
      (text-scale-set 0))))
(global-set-key (kbd "M-<f7>") #'centaur-read-mode)


(use-package calibredb
  :defer t
  :init
  ;; (autoload 'calibredb "calibredb")
  :config
  (setq calibredb-root-dir (concat lxs-home-dir "Documents/" "Calibre"))
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist (list calibredb-root-dir))
  (setq sql-sqlite-program "/usr/bin/sqlite3")
  (setq calibredb-program "/usr/bin/calibredb")
  (setq calibredb-format-all-the-icons t)
  (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  )

(use-package elfeed
    :pretty-hydra
    ((:title (pretty-hydra-title "Elfeed" 'faicon "rss-square" :face 'all-the-icons-orange :height 1.1 :v-adjust -0.05)
      :color amaranth :quit-key "q")
     ("Search"
      (("c" elfeed-db-compact "compact db")
       ("g" elfeed-search-update--force "refresh")
       ("G" elfeed-search-fetch "update")
       ("y" elfeed-search-yank "copy URL")
       ("+" elfeed-search-tag-all "tag all")
       ("-" elfeed-search-untag-all "untag all"))
      "Filter"
      (("l" elfeed-search-live-filter "live filter")
       ("s" elfeed-search-set-filter "set filter")
       ("*" (elfeed-search-set-filter "@6-months-ago +starred") "starred")
       ("a" (elfeed-search-set-filter "@6-months-ago") "all")
       ("t" (elfeed-search-set-filter "@1-day-ago") "today"))
      "Article"
      (("b" elfeed-search-browse-url "browse")
       ("n" next-line "next")
       ("p" previous-line "previous")
       ("u" elfeed-search-tag-all-unread "mark unread")
       ("r" elfeed-search-untag-all-unread "mark read")
       ;; ("j" my/elfeed-toggle-star "toggle star")
       ("R" elfeed-mark-all-as-read "mark all")
       ("k" my/elfeed-search-star "mark star")
       ("RET" elfeed-search-show-entry "show"))))
    :bind (("C-x w" . elfeed)
           :map elfeed-search-mode-map
           ("." . elfeed-hydra/body)
           :map elfeed-show-mode-map
           ("o" . ace-link)
           ("q" . delete-window))
    :hook (elfeed-show-mode . centaur-read-mode)
    :init (setq url-queue-timeout 30
                elfeed-db-directory (locate-user-emacs-file ".elfeed")
                elfeed-show-entry-switch #'pop-to-buffer
                elfeed-show-entry-delete #'delete-window
		)
  :custom ((elfeed-use-curl t)
           ;; (elfeed-curl-extra-arguments '("-x" "socks5h://localhost:8078"))
	   (elfeed-curl-extra-arguments '("-x" "http://localhost:20171"))
	   )
    :config
  ;; Ignore db directory in recentf
  (push elfeed-db-directory recentf-exclude)

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  (setq org-hide-leading-stars nil))

    ;; Use xwidget if possible
(with-no-warnings
  (defun my-elfeed-show-visit (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
    (interactive "P")
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
            (message "Sent to browser: %s" link)
            (cond
             ;; ((featurep 'xwidget-internal)
              ;; (centaur-webkit-browse-url link))
             (use-generic-p
              (browse-url-generic link))
             (t (browse-url link))))))
      (advice-add #'elfeed-show-visit :override #'my-elfeed-show-visit)

      (defun my-elfeed-search-browse-url (&optional use-generic-p)
        "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
        (interactive "P")
        (let ((entries (elfeed-search-selected)))
          (cl-loop for entry in entries
                   do (elfeed-untag entry 'unread)
                   when (elfeed-entry-link entry)
                   do (cond
                       ;; ((featurep 'xwidget-internal)
                        ;; (centaur-webkit-browse-url it t))
                       (use-generic-p
                        (browse-url-generic it))
                       (t (browse-url it))))
          (mapc #'elfeed-search-update-entry entries)
          (unless (or elfeed-search-remain-on-entry (use-region-p))
            (forward-line))))
      (advice-add #'elfeed-search-browse-url :override #'my-elfeed-search-browse-url))

(defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))

 (defun my/elfeed-toggle-star ()
    (interactive)
    (when elfeed-show-entry
      (let* ((tag (intern "starred"))
             (taggged (elfeed-tagged-p tag elfeed-show-entry)))
        (if taggged
            (elfeed-untag elfeed-show-entry tag)
          (elfeed-tag elfeed-show-entry tag))
        (message "Starred: %s" (not taggged)))))

 (defun my/elfeed-search-star ()
   (interactive)
	(let ((tag (intern "starred"))
          (entries (elfeed-search-selected)))
	  (cl-loop for entry in entries do (elfeed-tag entry tag))
	  (mapc #'elfeed-search-update-entry entries)
	  (unless (use-region-p) (forward-line)))))

(provide 'init-reader)
