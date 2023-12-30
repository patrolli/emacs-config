;; quick insert timestamp in texts
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-date-format "%a %b %d"
  "Format of date to insert with `insert-curent-date' func")

(defun xs/insert-current-date-time-with-line-split ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (let () (or comment-start "")))
  (insert "==========\n")
  (insert (let () (or comment-start "")))
  ;; (insert " ")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun xs/insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (let () (or comment-start "")))
  (insert (concat "[" (format-time-string current-date-time-format (current-time))) "]"))

(defun xs/insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n"))

(defun xs/insert-current-date ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-date-format (current-time)))
       (insert "\n"))

(defun xs/set-bookmark (&optional name)
  (interactive)
  (let ((filename (or name (format "%s:%s" (buffer-name) (line-number-at-pos)))))
    (bookmark-set filename)))

(provide 'personal)
