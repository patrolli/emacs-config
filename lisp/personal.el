;; scratch buffer 的代码有用的一定要记得保存，不然就找不回来了

(defun lxs/org-refile-headline-by-tag ()
  (interactive)
  (save-excursion
  (let ((tags (org-get-tags nil t))
	(headline  (nth 4 (org-heading-components)))
	remove-flag)
    ;; if the headline has no tag and being under "Un-archieve"
    (when (and (eq 0 (length tags))  (progn
				       (save-excursion
				       (outline-up-heading 1)
				       (string-equal "Un-archieve" (nth 4 (org-heading-components))))))
      (org-cut-subtree))
    ;; process each tag
    (while tags
	(let* ((tag (nth 0 tags))
	      (remove-flag nil))
	  (save-excursion
	    ;; if exist the level-1 headline of this tag
	    ;;;; narrow region to search the title under this tag headline
	    ;;;;;; if title exist
	    ;;;;;;;; prepare for remove the tag of current entry (set remove-flag)
	    ;;;;;; else
	    ;;;;;;;; insert this title under the tag headline
	    ;; else
	    ;;;; insert this new tag headline
	    ;;;; insert title under this new headline
	    ;; if remove-tag
	    ;;;; remove tag for current entry
	  (if (org-ql-select (buffer-name)
		`(and (level 1) (heading ,tag)))
	      (progn
		(let ((start (re-search-forward (concat "^* " tag "\n") nil t))
		      (end (re-search-forward "^* .*\n" nil t)))
		  (save-restriction
		    (setq start (if start start 1))
		    (setq end (if end end (point-max)))
		    (narrow-to-region start end)
		    (goto-char (point-min))
		    (if (re-search-forward (format "^** %s" (regexp-quote headline)) nil t) ;; no such titles under this tag headline
			;; remove the tag of current entry headline
			(progn
			  (setq remove-flag t)
			  )
		      (progn
			(goto-char (point-max))
			(forward-line -1)
			(insert (format "** %s\n" headline)))
		      )
		    )
		  ))
	    (progn
	      (end-of-buffer)
	      (forward-line)
	      (insert (format "* %s\n" tag))
	      (forward-line 2)
	      (insert (format "** %s\n" headline))
	      )
	    )
	  )
	  (if remove-flag
	      (org-toggle-tag tag 'off))
	)
	(setq tags (cdr tags))
	)
    )))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
     (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun lxs/open-compaction-file ()
  (interactive)
  (let ((my-compaction-file "/mnt/c/Users/lixun/Documents/org/org-roam-files/compaction.org"))
    (find-file my-compaction-file)))

(global-set-key (kbd "<f3>") #'lxs/open-compaction-file)

(defun lxs/org-find-datetree ()
  (interactive)
  (let ( (m (org-find-olp (cons (org-capture-expand-file "~/test.org") '("test"))) ))
    (set-buffer (marker-buffer m))
   (org-capture-put-target-region-and-position)
   (widen)
   (goto-char m)
   (set-marker m nil)
   (org-capture-put-target-region-and-position)
   (org-datetree-find-date-create (calendar-gregorian-from-absolute (org-today)) (when '("test") 'subtree-at-point))
   )
  )

(defun random-choice (items)
  (let* ((size (length items))
	 (index (random size)))
    (nth index items)))

(defun lxs/pick-random-theme ()
  (interactive)
  (load-theme (random-choice (custom-available-themes))))

;; (use-package which-function-mode
;;   :hook ((prog-mode . which-function-mode)
;;          (org-mode . which-function-mode))
;;   :init
;;   (setq which-func-unknown "")
;;   (add-hook 'which-function-mode-hook
;;             #'(lambda ()
;;                 (add-to-list 'which-func-functions
;;                              #'(lambda ()
;;                                  (when (eq major-mode 'org-mode)
;;                                    (mapconcat 'identity (org-get-outline-path t)
;;                                               \" > \"))))))
;;   )

;; (add-hook 'prog-mode-hook '(lambda () (setq header-line-format
;;                                        '((which-func-mode (\"\" which-func-format))))))
;; (add-hook 'org-mode-hook '(lambda () (setq header-line-format
;; 					   '((which-func-mode (\"\" which-func-format))))))

(defun lxs-org-is-hugo-file-p (fPath)
  "Predict if the org file has been converted into hugo"
  (with-temp-buffer
    (let ((keyline "#+HUGO_DRAFT: false\n"))
	  (insert-file-contents fPath)
	  (and (search-forward keyline nil t) t)
	  )
    ))

(setq lxs-org-hugo-header-info "#+HUGO_BASE_DIR: /mnt/c/Users/lixun/Documents/xssq-blog\n#+HUGO_AUTO_SET_LASTMOD: t\n#+HUGO_TAGS: \n#+HUGO_CATEGORIES: \n#+HUGO_DRAFT: false \n\n")

(defun lxs/org-hugo-export-current-buffer ()
  (interactive)
  (if (lxs-org-is-hugo-file-p (buffer-name))
      (org-hugo--export-file-to-md (buffer-name))
    (progn
      (beginning-of-buffer)
      (insert lxs-org-hugo-header-info)
      (org-hugo--export-file-to-md (buffer-name)
      ))
  ;; (org-hugo--export-file-to-md (buffer-name))
    ))

(defun lxs/org-find-project-journal-datetree ()
  ;; (interactive)
  (let* ((project (completing-read "Choose a project" '("compaction")))
	(m (org-find-olp (cons (org-capture-expand-file (concat lxs-home-dir "Documents/" "org/" "org-roam-files/" project ".org")) '("Journal")))))
    (set-buffer (marker-buffer m))
   ;; (org-capture-put-target-region-and-position)
   (widen)
   (goto-char m)
   (set-marker m nil)
   ;; (org-capture-put-target-region-and-position)
   (org-datetree-find-date-create (calendar-gregorian-from-absolute (org-today)) (when '("Journal") 'subtree-at-point))
   )
  )

(defun lxs/org-find-project-idea-datetree ()
  ;; (interactive)
  (let* ((project (completing-read "Choose a project" '("compaction")))
	(m (org-find-olp (cons (org-capture-expand-file (concat lxs-home-dir "Documents/" "org/" "org-roam-files/" project ".org")) '("Idea")))))
    (set-buffer (marker-buffer m))
   ;; (org-capture-put-target-region-and-position)
   (widen)
   (goto-char m)
   (set-marker m nil)
   ;; (org-capture-put-target-region-and-position)
   (org-datetree-find-date-create (calendar-gregorian-from-absolute (org-today)) (when '("Idea") 'subtree-at-point))
   )
  )

(add-to-list 'org-capture-templates `("w" "Project"))
;; (add-to-list 'org-capture-templates `("l" "lxs test" entry (function lxs/org-find-project-journal-datetree) "* %U - %^{heading}\n  %?"))
(add-to-list 'org-capture-templates `("wj" "project joural" entry (function lxs/org-find-project-journal-datetree) "* %U - %^{heading}\n  %?"))
(add-to-list 'org-capture-templates `("wi" "project idea" entry (function lxs/org-find-project-idea-datetree)	"* %U - %^{heading}\n  %?"))

(provide 'personal)
