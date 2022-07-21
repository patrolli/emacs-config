(defun xs-hugo-init-tag ()
  "Set hugo tag as the same with roam node tag"
  (interactive)
  (let* ((tags (split-string (or (cadr (assoc "FILETAGS"
					      (org-collect-keywords '("filetags")))) "") ":" 'omit-nulls)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "#\\+HUGO_TAGS:")
      (dolist (tag tags)
	(insert (format " \"%s\"" tag))))))

(defun xs-hugo-clear-tag ()
  "Clear hugo tags"
  (interactive)
  (save-excursion
      (goto-char (point-min))
      (re-search-forward "#\\+HUGO_TAGS:")
      (let* ((s (point))
	     (e (point-at-eol)))
	(delete-region s e))))

(provide 'init-hugo)
