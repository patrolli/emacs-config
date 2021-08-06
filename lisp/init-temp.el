;; (defun company-citre (-command &optional -arg &rest _ignored)
;;   "Completion backend of for citre.  Execute COMMAND with ARG and IGNORED."
;;   (interactive (list 'interactive))
;;   (cl-case -command
;;     (interactive (company-begin-backend 'company-citre))
;;     (prefix (and (bound-and-true-p citre-mode)
;;                  (or (citre-get-symbol) 'stop)))
;;     (meta (citre-get-property 'signature -arg))
;;     (annotation (citre-capf--get-annotation -arg))
;;     (candidates (all-completions -arg (citre-capf--get-collection -arg)))
;;     (ignore-case (not citre-completion-case-sensitive))))

(defun org-fast-todo-selection (&optional current-state)
  "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur.
When CURRENT-STATE is given and selection letters are not unique globally,
prefer a state in the current sequence over on in another sequence.
I modified this source code, to avoid reframe my window configuration when
using org-todo. I comment (delete-other-window) and set the * Org todo*
window size"
  (let* ((fulltable org-todo-key-alist)
	 (head (org-get-todo-sequence-head current-state))
	 (done-keywords org-done-keywords) ;; needed for the faces.
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (expert (equal org-use-fast-todo-selection 'expert))
	 (prompt "")
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 tg cnt e c tbl subtable
	 groups ingroup in-current-sequence)
    (save-excursion
      (save-window-excursion
	(if expert
	    (set-buffer (get-buffer-create " *Org todo*"))
	  ;; (delete-other-windows)
	  (set-window-buffer (split-window-vertically -10) (get-buffer-create " *Org todo*"))
	  (org-switch-to-buffer-other-window " *Org todo*"))
	(erase-buffer)
	(setq-local org-done-keywords done-keywords)
	(setq tbl fulltable cnt 0)
	(while (setq e (pop tbl))
	  (cond
	   ((equal e '(:startgroup))
	    (push '() groups) (setq ingroup t)
	    (unless (= cnt 0)
	      (setq cnt 0)
	      (insert "\n"))
	    (setq prompt (concat prompt "{"))
	    (insert "{ "))
	   ((equal e '(:endgroup))
	    (setq ingroup nil cnt 0 in-current-sequence nil)
	    (setq prompt (concat prompt "}"))
	    (insert "}\n"))
	   ((equal e '(:newline))
	    (unless (= cnt 0)
	      (setq cnt 0)
	      (insert "\n")
	      (setq e (car tbl))
	      (while (equal (car tbl) '(:newline))
		(insert "\n")
		(setq tbl (cdr tbl)))))
	   (t
	    (setq tg (car e) c (cdr e))
	    (if (equal tg head) (setq in-current-sequence t))
	    (when ingroup (push tg (car groups)))
	    (when in-current-sequence (push e subtable))
	    (setq tg (org-add-props tg nil 'face
				    (org-get-todo-face tg)))
	    (when (and (= cnt 0) (not ingroup)) (insert "  "))
	    (setq prompt (concat prompt "[" (char-to-string c) "] " tg " "))
	    (insert "[" c "] " tg (make-string
				   (- fwidth 4 (length tg)) ?\ ))
	    (when (and (= (setq cnt (1+ cnt)) ncol)
		       ;; Avoid lines with just a closing delimiter.
		       (not (equal (car tbl) '(:endgroup))))
	      (insert "\n")
	      (when ingroup (insert "  "))
	      (setq cnt 0)))))
	(insert "\n")
	(goto-char (point-min))
	(unless expert (org-fit-window-to-buffer))
	(message (concat "[a-z..]:Set [SPC]:clear"
			 (if expert (concat "\n" prompt) "")))
	(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
	(setq subtable (nreverse subtable))
	(cond
	 ((or (= c ?\C-g)
	      (and (= c ?q) (not (rassoc c fulltable))))
	  (setq quit-flag t))
	 ((= c ?\ ) nil)
	 ((setq e (or (rassoc c subtable) (rassoc c fulltable))
		tg (car e))
	  tg)
	 (t (setq quit-flag t)))))))

(defun lxs/fast-archive-done-tasks ()
  (interactive))

(defun ffmpeg-convert-webm-to-img ()
  (interactive)
  ;; 读入一个 video id
  (let* ((video-id (read-from-minibuffer "输入 video 的 id:\n" nil nil))
	 (video-fpath (format "/mnt/c/Users/lixun/Downloads/20bn-something-something-v2/%s.webm" video-id))
	 (target-path (concat "~/Documents/sth-demos/" video-id "/"))
	 (target-video-path (concat target-path video-id ".webm")))
    (if (not (f-directory-p target-path))
	(make-directory target-path nil)
      )
    (if (not (f-exists-p target-video-path))
	(progn (message target-video-path)
	       (copy-file video-fpath target-video-path))
      (message "视频已经在路径中")
      )
    (shell-command (format "ffmpeg -i %s %sframe_%%d.jpg" target-video-path target-path))
    (message "转换完成")
    ;; (message target-path)
    ;; (print (format "ffmpeg -i %s %sframe_%%d" target-video-path target-path))
  ;; 到数据集的目录下把视频文件复制到 ~/Documents/sth-demos 的文件夹下
  ;; C:\Users\lixun\Downloads\20bn-something-something-v2
  ;; 在文件夹下创建一个 video id 的 folder
  ;; ffmpeg -i ./video.webm ./video/image%d.jpg
  ))

;; (shell-command-to-string (format "ffmpeg -i %s  %sframe_%%d.jpg" "~/Documents/sth-demos/157655/157655.webm" "~/Documents/sth-demos/157655/"))

(setq org-roam-node-display-template "${tags}${filetitle}${olp}${title}")
(setq org-roam-node-display-template "(${tags})${title}")
(defun org-roam-node--format-entry (node width)
    "Formats NODE for display in the results list.
WIDTH is the width of the results list.
Uses `org-roam-node-display-template' to format the entry."
    (let ((fmt (org-roam--process-display-format org-roam-node-display-template)))
      (org-roam-format
       (car fmt)
       (lambda (field)
         (let* ((field (split-string field ":"))
                (field-name (car field))
                (field-width (cadr field))
                (getter (intern (concat "org-roam-node-" field-name)))
                (field-value (or (funcall getter node) "")))
           (when (and (equal field-name "tags")
                      field-value)
             (setq field-value (org-roam--tags-to-str field-value)))
           (when (and (equal field-name "file")
                      field-value)
             (setq field-value (file-relative-name field-value org-roam-directory)))
           (when (and (equal field-name "olp")
                      field-value)
             (setq field-value (if (> (length field-value) 0)
                                   (format "%s > " (string-join field-value " > "))
                                 ""))
	     )
           (if (not field-width)
               field-value
             (setq field-width (string-to-number field-width))
             (truncate-string-to-width
              field-value
              (if (> field-width 0)
                  field-width
                (- width (cdr fmt)))
              0 ?\s)))))))

  (defun org-roam--tags-to-str (tags)
    "Convert list of TAGS into a string."
    (if (> (length tags) 0)
        (format "(%s) " (mapconcat (lambda (s) (concat "" s)) tags ","))
      ""))

  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (let ((filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
          (title (org-roam-node-title node)))
      (if (string= filetitle title)
          ""
        (format "%s > " filetitle))))

  (defun org-roam-node--to-candidate (node)
    (let ((candidate-main (org-roam-node--format-entry node (1- (frame-width)))))
      (cons (propertize candidate-main 'node node) node)))

  (defun org-roam-get-keyword (name &optional file bound)
  "Return keyword property NAME from an org FILE.
FILE defaults to current file.
Only scans up to BOUND bytes of the document."
  (unless bound
    (setq bound 1024))
  (if file
      (with-temp-buffer
        (insert-file-contents file nil 0 bound)
        (org-roam--get-keyword name))
    (org-roam--get-keyword name bound)))

(defhydra hydra-org-clock (:color pink :hint nil)
"
org-clock hydra key

clock                             ^^^^effort             ^^watcher
-------------------------------^^^^^^^---------------------------------
[_i_]  clock in     [_c_]  cancel     [_e_] set effort     [_t_] toggle
[_L_]  clock last   [_o_]  clock out  [_E_] reset effort   [_s_] start
[_r_]  resolve                                         ^^^^[_S_] stop
[_g_]  goto                                            ^^^^[_w_] status
[_J_]  jump2current                                    ^^^^[_O_] open plan

[_q_] cancel
"
      ("i" org-clock-in)
      ("o" org-clock-out :exit t)
      ("r" org-resolve-clocks :exit t)
      ("g" org-clock-goto :exit t)
      ("J" org-clock-jump-to-current-clock :exit t)
      ("c" org-clock-cancel :exit t)
      ("L" org-clock-in-last)
      ("e" org-set-effort :exit t)
      ("E" org-clock-modify-effort-estimate :exit t)
      ("t" org-clock-watch-toggle :exit t)
      ("s" (org-clock-watch-toggle 'on) :exit t)
      ("S" (org-clock-watch-toggle 'off) :exit t)
      ("w" (org-clock-watch-status))
      ("O" org-clock-watch-goto-work-plan)
      ("q" nil :color blue))

(defun my-swap-org-ref-cite-title ()
  (interactive)
  (save-restriction
    (beginning-of-line)
    (let* ((lbegin (line-beginning-position 1))
	   (lend (line-beginning-position 2))
	   (ip (re-search-forward "\\(*.? \\|- \\)" nil t nil))
	   (cp (re-search-forward "cite:.*? " nil t nil))
	   )   
      (narrow-to-region lbegin lend)
      (beginning-of-line)
      (kill-region cp (- lend 1))
      (goto-char ip)
      (yank)
      (insert " ")
      )
    )
  )

(replace-regexp-in-string "\\\(<\\\|>\\\|-\\\)" "_" "<untitled-1>")
