(defun company-citre (-command &optional -arg &rest _ignored)
  "Completion backend of for citre.  Execute COMMAND with ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case -command
    (interactive (company-begin-backend 'company-citre))
    (prefix (and (bound-and-true-p citre-mode)
                 (or (citre-get-symbol) 'stop)))
    (meta (citre-get-property 'signature -arg))
    (annotation (citre-capf--get-annotation -arg))
    (candidates (all-completions -arg (citre-capf--get-collection -arg)))
    (ignore-case (not citre-completion-case-sensitive))))

(setq company-backends '((company-capf company-citre :with company-yasnippet :separate)))



(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )
