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
     '(display "◦"))
    ;; (when (match-beginning 2)
    ;;   (pcase (match-string-no-properties 2)
    ;;     ("[-] " (gkroam--fontify-org-checkbox "□"))
    ;;     ("[ ] " (gkroam--fontify-org-checkbox "□"))
    ;;     ("[X] " (gkroam--fontify-org-checkbox "🗹"))))
    ))

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

(defun xs-locate-doing-headlines ()
  (interactive)
  "列出 next.org 中的 headline, 选择之后，跳到 headline 区域的最后位置"
  (let* ((entry-info (xs-list-doing-tasks))
	 (i 0)
	 (headlines (mapcar #'(lambda (x) (setq i (+ 1 i))
				(format "%s-%s" i (car x))) entry-info))
	 (chosen (completing-read "choose headline: " headlines))
	 (idx (-elem-index chosen headlines))
	 (marker (cdr (nth idx entry-info))))
    (set-buffer (org-base-buffer (marker-buffer marker)))
    (goto-char marker)
    (goto-char (org-entry-end-position))))

(defun xs-org-roam-remove-refile-tag ()
  "移出当前 roam node 的 refile tag"
  (interactive)
  (org-roam-tag-remove '("refile"))
  (save-buffer))

(defun iso-week-from-date (month day year)
  "从日期转换为ISO时间标准周"
  (calendar-iso-from-absolute
   (calendar-absolute-from-gregorian (list month day year))))

(defun xs-archive-subtree-to-daily ()
  "archive DONE tasks into dailies according to its CLOSED time
;[2022-06-17 Fri 15:06] first version"
  (interactive)
  (let* ((datetree-date (or (org-entry-get nil "CLOSED" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         (year (nth 2 date))
         (month (nth 0 date))
         (day (nth 1 date))
         (week (nth 0 (apply #'iso-week-from-date date)))
         (buf (find-file-noselect (format "~/Documents/org/org-roam-files/daily/%s/week_%s.org" year week)))
         (tgt (format "%02d-%02d, %s" month day (format-time-string "%a" (date-to-time datetree-date))))
         )
    (org-copy-subtree)
    (let* ((head-info (nth 0 (org-ql-select buf
                               `(heading ,tgt)
                               :action #'(lambda () (progn
                                                      (let* ((heading (nth 4 (org-heading-components)))
                                                             (marker (xs-get-entry-marker)))
                                                        (cons heading marker))))))))
      (if head-info
          ;; paste current entry under this headline
          (progn
            (with-current-buffer (org-base-buffer (marker-buffer (cdr head-info)))
              (goto-char (cdr head-info))
	      (setq lvl (org-current-level))
              (goto-char (org-entry-end-position))
              (org-paste-subtree (+ lvl 1))))
        ;; if this headline does not exits, create and then paste under it
        (with-current-buffer buf
          (end-of-buffer)
	  (insert "\n")
          (org-insert-heading)
	  (if (and (org-current-level) (> (org-current-level) 1))
	    (org-promote-subtree))
          (insert (format "%s\n" tgt))
	  (setq lvl (org-current-level))
          (org-paste-subtree (+ lvl 1)))))))

(advice-add #'org-archive-subtree :before #'xs-archive-subtree-to-daily)

(defun xs/org-set-agenda-group ()
  "Set AGENDA-GROUP property for a headline under the cursor.
AGENDA-GROUP is used for displaying org ql dynamic block"
  (interactive)
  (let* ((value (org-read-property-value "AGENDA-GROUP")))
    (org-set-property "AGENDA-GROUP" value)))

(provide 'init-org-utils)
