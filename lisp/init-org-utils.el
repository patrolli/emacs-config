;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;          prettify org list           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar xs-org-list-prettify nil
  "toggle org list prettify")

(defvar gkroam-org-list-re
  "^ *\\([*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Org list bullet and checkbox regexp.")

(defun gkroam--fontify-org-checkbox (notation)
  "Highlight org checkbox with NOTATION."
  (add-text-properties
   (match-beginning 2) (1- (match-end 2)) `(display ,notation)))

(defun gkroam--fontify-org-list ()
  "Highlight org list, including bullet and checkbox."
  (with-silent-modifications
    (add-text-properties
     (match-beginning 1) (match-end 1)
     '(display "∙"))
    (when (match-beginning 2)
      (pcase (match-string-no-properties 2)
        ("[-] " (gkroam--fontify-org-checkbox "□"))
        ("[ ] " (gkroam--fontify-org-checkbox "□"))
        ("[X] " (gkroam--fontify-org-checkbox "🗹"))))))

(defun gkroam-org-list-fontify (beg end)
  "Highlight org list bullet between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward gkroam-org-list-re end t)
      (if (string= (match-string-no-properties 1) "*")
          (unless (= (match-beginning 0) (match-beginning 1))
            (gkroam--fontify-org-list))
        (gkroam--fontify-org-list)))))

(defun org-list-prettify ()
  (setq xs-org-list-prettify t)
  (jit-lock-register #'gkroam-org-list-fontify)
  (gkroam-org-list-fontify (point-min) (point-max)))

(defun toggle-org-list-prettify ()
  (interactive)
  (if xs-org-list-prettify
      (progn
	(setq xs-org-list-prettify nil)
	(jit-lock-unregister #'gkroam-org-list-fontify)
	(save-excursion
          (goto-char (point-min))
          (while (re-search-forward gkroam-org-list-re nil t)
            (with-silent-modifications
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(display nil))))))
    (org-list-prettify))
  (jit-lock-refontify))

(add-hook 'org-mode-hook #'org-list-prettify)



(provide 'init-org-utils)
