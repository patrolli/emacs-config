(require 'org)
(require 'org-clock)
(require 'alert-toast)

(setq org-clock-x11idle-program-name "xprintidle")

(defvar org-clock-watch-clock-in-notify-interval
  180 "提醒设置 clock 的时间间隔")

(defvar org-clock-watch-idle-threshold-minutes  
  "1min" "当计算机的空闲时间小于这个时间时，才会发送提醒")

(defvar org-clock-watch-timer nil "the timer that runs org-clock-watcher")

(defvar org-clock-watch-total-on-time 0 "total time (sec) since turn on watch")

(defvar org-clock-watch-overflow-thredshold "2h" "任务计时超过这个阈值，发送提醒")

(defvar org-clock-watch-auto-clock-out-seconds 1800 "当计算机空闲时间超过半小时，就自动关闭 clock")

(defun xs-org-clock-watcher ()
  ;; tik-tok
  (setq org-clock-watch-total-on-time (1+ org-clock-watch-total-on-time))
  ;; 当计算机长时间处于空闲状态时，自动关闭任务
  (when (and (org-clocking-p)
             (> (org-x11-idle-seconds) org-clock-watch-auto-clock-out-seconds))
    (ignore-errors (org-clock-out nil t (time-subtract (current-time) (org-x11-idle-seconds))))
    (message "org-clock-watch: auto clock out due to idle"))
  
  (when (< (org-x11-idle-seconds) (* 60 (org-duration-to-minutes org-clock-watch-idle-threshold-minutes)))
    (if (not (org-clocking-p))
	;; 如果当前没有计时
    (when (zerop (mod org-clock-watch-total-on-time org-clock-watch-clock-in-notify-interval))
      (alert-toast-notify '(:title "clock in?" :message "当前没有设置 clock" :data (:long t))))
    ))
  )

(defun xs-org-clock-watch-toggle ()
  "开始或暂停 xs-org-clock-watcher"
  (interactive)
  (if org-clock-watch-timer
      (setq org-clock-watch-timer (cancel-timer org-clock-watch-timer))
    (setq org-clock-watch-timer (run-with-timer 5 1 'xs-org-clock-watcher)))
  (if org-clock-watch-timer
      (message "org-clock-watcher started")
    (message "org-clock-watcher stopped"))
  )


(defun xs-org-clock-watch-status ()
  "get the status of watcher"
  (interactive)
  (if org-clock-watch-timer
      (message "org-clock-watcher is running")
    (message "org-clock-watcher is stopped")))

(provide 'xs-org-clock-watch)
