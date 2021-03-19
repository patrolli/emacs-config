(require 'init-const)
(require 'init-custom)

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

(provide 'init-reader)
