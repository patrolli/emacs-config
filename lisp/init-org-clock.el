(require 'org-ql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;         org idle timer watch        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto clock out taks when idle
(defvar xs-org-clock-out-idle-threshold 600
  "threshold to auto clock out a task")

(defvar xs-org-clock-idle-watch-timer nil)

(defun xs-org-clock-idle-watch ()
  (when (and (org-clocking-p)
	     (> (org-x11-idle-seconds) xs-org-clock-out-idle-threshold))
    (ignore-errors (org-clock-out nil t (time-subtract (current-time) (org-x11-idle-seconds))))
    (message "auto clock out due to idle")))

(defun xs-org-clock-idle-watch-toggle ()
  (interactive)
  (if xs-org-clock-idle-watch-timer
      (setq xs-org-clock-idle-watch-timer (cancel-timer xs-org-clock-idle-watch-timer))
    (setq xs-org-clock-idle-watch-timer (run-with-timer 5 1 'xs-org-clock-idle-watch))
    )
  (if xs-org-clock-idle-watch-timer
      (message "org clock idle watch started")
    (message "org clock idle watch stopped")))

(xs-org-clock-idle-watch-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;           list doing tasks          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-ql-select 在定义自己的 action function 的时候
;; 似乎还有一些问题，现在只能写成 lambda 的形式
(defun xs-list-doing-tasks ()
  "get a list of DOING state entries
return their headline and marker position"
  (let* ((headings (org-ql-select (org-agenda-files)
		   '(todo "DOING")
		   :action #'(lambda () (progn
					  (let* ((heading (nth 4 (org-heading-components)))
						 (marker (xs-get-entry-marker)))
					    (cons heading marker)))))))
    headings))

(defun xs-get-entry-marker (&optional pos buffer)
  "Return the entry marker."
  (let ((m (move-marker (make-marker)
			(or pos (point)) (org-base-buffer
					  (or buffer (current-buffer))))))
    m))

(defun xs-clock-in-doing-tasks ()
  (interactive)
  (let* ((entry-info (xs-list-doing-tasks))
	 (i 0)
	 (headlines (mapcar #'(lambda (x) (setq i (+ 1 i))
				(format "%s-%s" i (car x))) entry-info))
	 (chosen (completing-read "choose a task: " headlines))
	 (idx (-elem-index chosen headlines))
	 (marker (cdr (nth idx entry-info))))
    (with-current-buffer (org-base-buffer (marker-buffer marker))
      (goto-char marker)
      (org-clock-in))))

(provide 'init-org-clock)
