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
    ))

(defun ffmpeg-convert-webm-to-gif ()
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
	       (copy-file video-fpath target-video-path)
	       (message "视频已经在路径中")
	       )
      )
    (shell-command (format "ffmpeg -i %s -t 5 %s%s.gif" target-video-path target-path video-id) nil)
    (message "转换完成")
  ))

;; (shell-command-to-string (format "ffmpeg -i %s  %sframe_%%d.jpg" "~/Documents/sth-demos/157655/157655.webm" "~/Documents/sth-demos/157655/"))

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

(replace-regexp-in-string "\\\(<\\\|>\\\|-\\\)" "_" "<untitled-1>")

(use-package org-sidebar)

(defun my/roam-init-node ()
  "init org-roam headline node"
  (interactive)
  (progn (org-id-get-create)
         (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))

(defun reverie-brother-headline ()
  "insert same level headline node"
  (interactive)
  (progn (org-insert-heading-respect-content)
         (org-id-get-create)
         (org-entry-put nil "CREATED" (iso8601-format (current-time)))))

(defun reverie-child-headline ()
  "insert next level headline node"
  (interactive)
  (progn (org-insert-heading-respect-content)
         (org-do-demote)
         (org-id-get-create)
         (org-entry-put nil "CREATED" (iso8601-format (current-time)))))

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

(defun joseph-kill-all-other-dired-buffers ( &optional current-buf)
  "kill all dired-buffers and diredp-w32-drivers-mode(w32 use this mode )
  except current-buf ,if current-buf is nil then kill all"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (not (eq current-buf buf))
                 (or  (eq 'dired-mode  major-mode)
                      (eq 'diredp-w32-drives-mode major-mode)))
        (kill-buffer buf)))))

(defadvice dired (before dired-single-buffer activate)
  "Replace current buffer if file is a directory."
  (joseph-kill-all-other-dired-buffers)
  )

(advice-remove 'joseph-kill-all-other-dired-buffers #'dired)
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(setq ebib-preload-bib-files '("/mnt/c/Users/lixun/Documents/bibliography/library.bib"))

(advice-add #'elfeed-insert-html
              :around
              (lambda (fun &rest r)
                (let ((shr-use-fonts nil))
                  (apply fun r))))

(setq org-roam-node-display-template "${my-tag}${filetitle}${olp}${title}")

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
                                 "")))
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

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node)))))
    (format "[%d]" count)))

(setq org-roam-node-display-template "${my-tag}${filetitle}${olp}${title}")

;; 将 bibtex 文件的 entry 中的 keyword 值，同步到 org-roam 的 filetags 里面
(defun bu-make-field-keywords (&optional arg)
  "Make a keywords field.
If ARG is nil, ask for each keyword and offer completion over
keywords that are already available in the buffer.  Inserting
the empty string will quit the prompt. If the keyword is not already
present in the buffer, it will be added to the local variable
bu-keywords-values. Note that if you use ido-ubiquitous, the value of
  `ido-ubiquitous-enable-old-style-default' is temporarily set to t within
the body of this command."
  (interactive "P")
  (let ((elist (save-excursion (bibtex-beginning-of-entry)
			       (bibtex-parse-entry)))
        (ido-ubiquitous-enable-old-style-default t)
	append)
    (if (assoc "keywords" elist)
	(progn (setq append t)
	       (bibtex-beginning-of-entry)
	       (goto-char
		(car (last (or (bibtex-search-forward-field "keywords" t)
                               (progn (setq append nil)
                                      (bibtex-search-forward-field "OPTkeywords" t)))))))
      (bibtex-make-field "keywords" t nil))
    (skip-chars-backward "}\n")
    (unless arg
      (let ((cnt 0)
            k)
	(while (and (setq k (completing-read
                             "Keyword (RET to quit): " bu-keywords-values nil))
		    (not (equal k "")))
	  (when append (insert ", ")
                (setq append nil))
	  (setq cnt (1+ cnt))
	  (insert (format "%s%s" (if (> cnt 1) ", " "") k))
          (add-to-list 'bu-keywords-values k))))))

;; 从 org buffer 中读取 cite-key
;; 提取其中 keyword 的内容
;; 插入到 org-roam 的 file tag 中
;; 2021-09-20: 获取 cite 的方式从 ROAM_KEY 换成 ROAM_REFS
(defun lxs/set-roam-tags-from-bib ()
    (interactive)
    (let* ((cite-key (substring (car (org-property-values "ROAM_REFS")) 5))
	   (current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                        (org-collect-keywords '("filetags"))))
                                           "")
                                       ":" 'omit-nulls))
	  (keywords (lxs-parse-bib-keywords cite-key)))
      (org-roam-set-keyword "filetags" (org-make-tag-string (seq-uniq (append keywords current-tags))))
      )
    )

(defun my/test ()
  (interactive)
  ;; 如果当前行不是 begin_src 行，那么向后查找到这行
  ;; 否则，直接执行 toggle 操作
  (save-excursion
    (beginning-of-line)
    (if (looking-at "#\\+begin_src")
	(progn
	  (message "在 begin_src 行")
	  (org-hide-block-toggle)
	  )
      (re-search-backward "#\\+begin_src")
      (org-hide-block-toggle))))

(defun lxs-parse-bib-keywords (cite-key)
  (with-temp-buffer
    (insert (org-ref-get-bibtex-entry cite-key))
    (let ((elist (save-excursion (bibtex-beginning-of-entry)
				 (bibtex-parse-entry)))
	  (keyword-regex "\\([a-zA-z\\-]+\\)"))
      (if-let* ((keywords-str (cdr (assoc "keywords" elist)))
		(keywords (re-seq keyword-regex keywords-str))
		)
	  keywords
	nil)))
  )

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(setq org-directory (concat lxs-home-dir "Documents/" "org/"))

(require 'nox)
(defun nox-imenu ()
  "Nox's `imenu-create-index-function'."
  (cl-labels
      ((visit (_name one-obj-array)
              (imenu-default-goto-function
               nil (car (nox--range-region
                         (nox--dcase (aref one-obj-array 0)
                           (((SymbolInformation) location)
                            (plist-get location :range))
                           (((DocumentSymbol) selectionRange)
                            selectionRange))))))
       (unfurl (obj)
               (nox--dcase obj
                 (((SymbolInformation)) (list obj))
                 (((DocumentSymbol) name children)
                  (cons obj
                        (mapcar
                         (lambda (c)
                           (plist-put
                            c :containerName
                            (let ((existing (plist-get c :containerName)))
                              (if existing (format "%s::%s" name existing)
                                name))))
                         (mapcan #'unfurl children)))))))
    (mapcar
     (pcase-lambda (`(,kind . ,objs))
       (cons
        (alist-get kind nox--symbol-kind-names "Unknown")
        (mapcan (pcase-lambda (`(,container . ,objs))
                  (let ((elems (mapcar (lambda (obj)
                                         (list (plist-get obj :name)
                                               `[,obj] ;; trick
                                               #'visit))
                                       objs)))
                    (if container (list (cons container elems)) elems)))
                (seq-group-by
                 (lambda (e) (plist-get e :containerName)) objs))))
     (seq-group-by
      (lambda (obj) (plist-get obj :kind))
      (mapcan #'unfurl
              (jsonrpc-request (nox--current-server-or-lose)
                               :textDocument/documentSymbol
                               `(:textDocument
                                 ,(nox--TextDocumentIdentifier))
                               :cancel-on-input non-essential))))))

(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-node-find 'other-window title nil)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

(use-package desktop
  :hook
  (after-init . desktop-read)
  (after-init . desktop-save-mode)
  :custom
  (desktop-restore-eager 4))

(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an org-mode buffer"
  (interactive)
  (beginning-of-buffer)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$")
  )

(use-package sort-tab
  :load-path "~/.emacs.d/site-lisp/sort-tab"
  :config
  (sort-tab-mode 1))

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam-ui")
(load-library "org-roam-ui")

;; 将 wsl 的文件前缀转成 windows 的文件前缀
;; 例如: /mnt/c/Users/lixun/Documents -> C:\\Users\\lixun\\Documents
(defun lxs/convert-wsl-prefix-path ()
  (interactive)
  ;; 先找到 [[file: 的行
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\\[\\[file:/mnt" nil t)
      (let* ((fpath (thing-at-point 'line t))
	     (wsl-prefix "/mnt/c/Users/lixun/")
	     (win-prefix "C:\\\\Users\\\\lixun\\\\")
	     (win-fpath (replace-regexp-in-string wsl-prefix win-prefix fpath)))
	(end-of-line)
	(insert (format "\n%s" (replace-regexp-in-string "/" "\\\\" win-fpath)))
	)
      )
    )
  )

;; 运行 hugo 文件夹下面的 deploy.sh, 发布博客
;; 把 easy-hugo 的 `easy-hugo-github-deploy' 拿过来改的
(defun lxs/deploy-hugo ()
  (interactive)
  (let* ((hugo-url "https://patrolli.github.io/xssq/")
	 (github-deploy-script "deploy.sh")
	 (hugo-base-dir "/mnt/c/Users/lixun/Documents/xssq-blog/")
	 (default-directory hugo-basedir)
	 (deployscript (file-truename (expand-file-name
				       github-deploy-script
				       hugo-base-dir))))
    (print deployscript)
    (unless (executable-find deployscript)
      (error "%s do not execute" deployscript))
    (let ((ret (call-process deployscript nil "*hugo-github-deploy*" t)))
       (unless (zerop ret)
	 (switch-to-buffer (get-buffer "*hugo-github-deploy*"))
	 (error "%s command does not end normally" deployscript)))
     ;; (when (get-buffer "*hugo-github-deploy*")
       ;; (kill-buffer "*hugo-github-deploy*"))
     (message "Blog deployed")
     (when hugo-url
       (browse-url hugo-url))
    ))

(use-package grammatical-edit
  :load-path "~/.emacs.d/site-lisp/grammatical-edit"
  :config
  (dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'python-mode-hook
               ))
    (add-hook hook '(lambda () (grammatical-edit-mode 1))))

  (define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
  (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
  (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
  (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
  (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
  (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
  (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

  (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
  (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
 
  (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
  (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

  (define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

  (define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
  (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
  (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
  (define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
  (define-key grammatical-edit-mode-map (kbd "M-)") 'grammatical-edit-unwrap)

  (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
  (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
  (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)
  )


;; custom dogears with counsel completing backend
(defun counsel-dogears ()
  (interactive)
  (ivy-read "Dogear list: "
            (cl-loop for place in dogears-list
                     collect (cons (dogears--format-record place)
                                   place))
	    :action (lambda (x) (dogears-go x))
	    )
  )

;; (defun counsel-roam-tag-add ()
  ;; (interactive)
  ;; (ivy-read "Complete roam tags: "
	    ;; (org-roam-tag-completions)
	    ;; )
  ;; :action 
  ;; )

(defun lxs/org-roam-tag-add (tags)
  "Add TAGS to the node at point."
  (interactive
   (list (ivy-completing-read "Tag: " (org-roam-tag-completions))))
  (let ((node (org-roam-node-at-point 'assert)))
    (save-excursion
      (goto-char (org-roam-node-point node))
      (if (= (org-outline-level) 0)
          (let ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                             (org-collect-keywords '("filetags"))))
                                                "")
                                            ":" 'omit-nulls)))
            (org-roam-set-keyword "filetags" (org-make-tag-string (seq-uniq (append tags current-tags)))))
        (org-set-tags (seq-uniq (append tags (org-get-tags)))))
      tags)))


(defun my/test ()
  (interactive)
  (with-output-to-temp-buffer "*lxs*"
    (let* ((object (org-element-context))
	   (end-pos (org-element-property :end object))
	   (link-string (org-element-property :path object))
	   (data (org-ref-parse-cite-path link-string))
	   (references (plist-get data :references))
	   (key (plist-get (car references) :key)))
      (print (format "%s" end-pos))
      (print "\n")
      )
    )
  )

(use-package taxy
  :load-path "~/.emacs.d/site-lisp/")

(require 'taxy)

(defvar numbery
  (make-taxy
   :name "Numbery"
   :description "A silly taxonomy of numbers."
   :taxys (list (make-taxy
                 :name "< 10"
                 :description "Numbers below 10 (consuming)"
                 :predicate (lambda (n) (< n 10))
                 :taxys (list
                         ;; These sub-taxys further classify the numbers below 10 into odd
                         ;; and even.  The odd taxy "consumes" numbers, while the even one
                         ;; doesn't, leaving them to reappear in the parent taxy's items.
                         (make-taxy :name "Odd"
                                    :description "(consuming)"
                                    :predicate #'oddp)
                         (make-taxy :name "Even"
                                    :description "(non-consuming)"
                                    :predicate #'evenp
                                    :then #'identity)))
                (make-taxy
                 :name ">= 10"
                 :description "Numbers above 9 (consuming)"
                 :predicate (lambda (n) (>= n 10))
                 :taxys (list
                         ;; Like in the "< 10" taxy, these sub-taxys further classify
                         ;; the numbers, but only one of them consumes numbers it
                         ;; takes in, leaving the rest to reappear in the parent taxy.
                         (make-taxy :name "Divisible by 3"
                                    :description "(non-consuming)"
                                    :predicate (lambda (n) (zerop (mod n 3)))
                                    :then #'identity)
                         (make-taxy :name "Divisible by 4"
                                    :description "(non-consuming)"
                                    :predicate (lambda (n) (zerop (mod n 4)))
                                    :then #'identity)
                         (make-taxy :name "Divisible by 3 or 4"
                                    :description "(consuming)"
                                    ;; Since this taxy's `:then' function is unset,
                                    ;; it defaults to `ignore', which causes it to
                                    ;; consume numbers it takes in.  Since these
                                    ;; numbers have already been taken in (without
                                    ;; being consumed) by the previous two sibling
                                    ;; taxys, they also appear in them.
                                    :predicate (lambda (n) (or (zerop (mod n 3))
                                                               (zerop (mod n 4)))))
                         (make-taxy :name "Divisible by 5"
                                    :description "(non-consuming)"
                                    :predicate (lambda (n) (zerop (mod n 5)))
                                    :then #'identity))))))

(let ((numbers (cl-loop for i below 100 collect i))
      ;; Since `numbery' is stored in a variable, we use an emptied
      ;; copy of it to avoid mutating the original taxy.
      (taxy (taxy-emptied numbery)))
  (taxy-plain (taxy-fill (reverse numbers) taxy)))
