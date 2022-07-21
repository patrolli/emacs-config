;; -*- lexical-binding: t -*-

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
	 (video-fpath (format "/run/media/lixunsong/Elements SE/20bn-something-something-v2/20bn-something-something-v2/%s.webm" video-id))
	 (target-path (concat "~/Documents/sth-demos/" video-id "/"))
	 (target-video-path (concat target-path video-id ".webm")))
    (if (not (f-directory-p target-path))
	(make-directory target-path nil))
    (if (not (f-exists-p target-video-path))
	(progn (message target-video-path)
	       (copy-file video-fpath target-video-path))
      (message "视频已经在路径中"))
    (shell-command (format "ffmpeg -i %s %sframe_%%d.jpg" target-video-path target-path))
    (message "转换完成")))

(f-exists? "/run/media/lixunsong/Elements SE/")
(f-exists? (format "/run/media/lixunsong/Elements SE/20bn-something-something-v2/20bn-something-something-v2/%s.webm" 99999))

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
  (with-output-to-temp-buffer "*lxs*"
    (print  (org-in-src-block-p))))

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
	(insert (format "\n%s" (replace-regexp-in-string "/" "\\\\" win-fpath)))))))

					; dogears ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dogears
  :ensure t)

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
  :load-path "site-lisp/taxy")

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

 ;; At this point the desktop.el hook in after-init-hook was
  ;; executed, so (desktop-read) is avoided.
  (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
    ;; Here we activate the desktop mode
    (desktop-save-mode 1)

    ;; The default desktop is saved always
    (setq desktop-save t)

    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)

    ;; Set the location to save/load default desktop
    (setq desktop-dirname user-emacs-directory)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    ;; (add-hook 'find-file-hook
     ;; (lambda ()
       ;; (run-with-timer 5 nil
          ;; (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            ;; (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            ;; (desktop-save user-emacs-directory)))))

    ;; Read default desktop
    (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook
              (lambda ()
                ;; Reset desktop modification time so the user is not bothered
                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
    )

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-at-point)
  :config
  (setq helpful-max-buffers 5)
  ;; don't pop new window
  (setq helpful-switch-buffer-function
        (lambda (buf) (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
                          ;; ensure the helpful window is selected for `helpful-update'.
                          (select-window window)
                        ;; line above returns nil if no available window is found
                        (pop-to-buffer buf))))
  (defvar moon-helpful-history () "History of helpful, a list of buffers.")
  (advice-add #'helpful-update :around #'moon-helpful@helpful-update)
  (advice-add #'helpful--buffer :around (lambda (oldfunc &rest _)
                                          (let ((buf (apply oldfunc _)))
                                            (push buf moon-helpful-history)
                                            buf))))

(defun moon-helpful@helpful-update (oldfunc)
  "Insert back/forward buttons."
  (funcall oldfunc)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (insert-text-button "Back"
                        'action (lambda (&rest _)
                                  (interactive)
                                  (lxs-helpful-cycle-buffer (current-buffer) 1)))
    (insert " / ")
    (insert-text-button "Forward"
                        'action (lambda (&rest _)
                                  (interactive)
                                  (lxs-helpful-cycle-buffer (current-buffer)  -1)))
    (insert "\n\n")))
;; 这里没有必要自己维护一个 helpful buffer 的历史，只需要 cycle
;; helpful buffer 就可以了。

(defvar lxs-helpful-cur-bufs nil
  "记录当前有哪些 helpful buffers")

(defun lxs-helpful-cycle-buffer (buffer &optional offset)
  (interactive)
  (let* ((buffers (or lxs-helpful-cur-bufs (buffer-list)))
	 (helpful-bufs (--filter (with-current-buffer it
                                   (eq major-mode 'helpful-mode))
				 buffers))
	 (idx (+ (or offset 0) (-elem-index buffer helpful-bufs)))
	 )
    (setq lxs-helpful-cur-bufs helpful-bufs)
    (if (< idx 0)
	(switch-to-buffer (nth (+ idx helpful-max-buffers) helpful-bufs))
      (switch-to-buffer (nth idx helpful-bufs))
      )
    )
  )

(setq tab-bar-border nil
      tab-bar-close-button nil
      tab-bar-back-button nil
      tab-bar-new-button nil
      tab-bar-format '(tab-bar-format-tabs +tab-bar-right)
      tab-bar-tab-name-format-function '+tab-bar-tab-format-function)

(defun +tab-bar-right ()
  (let* ((p (cdr (project-current)))
         (vc (+vc-branch-name))
         (w (string-width (concat p " " vc))))
    (concat (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,w 1))))
            p
            " "
            vc)))

(defun +tab-bar-switch-project ()
  "Switch to project in a new tab, project name will be used as tab name.
No tab will created if the command is cancelled."
  (interactive)
  (let (succ)
    (unwind-protect
        (progn
          (tab-bar-new-tab)
          (call-interactively #'project-switch-project)
          (when-let ((proj (project-current)))
            (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
            (setq succ t)))
      (unless succ
        (tab-bar-close-tab)))))

(defun +tab-bar-tab-format-function (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (concat
     (propertize (concat
                  " "
                  (alist-get 'name tab)
                  " ")
                 'face
                 (funcall tab-bar-tab-face-function tab))
     " ")))

(tab-bar-mode 1)
(org-time-since '(25014 61524 800003 174000))
(org-time-since 174000)
(time-since 174000)
(time-subtract nil 25014)

(setq ivy-dispatching-done-hydra-exit-keys '(("M-o" hydra-pop "back")
 ("C-g" hydra-pop)))

(require 'tab-line)

;; 指定函数返回一个 list 的 tabs
;; 要么是 buffer 的 list, 要么是要显示的 string 的 list
tab-line-tabs-function

(let ((group (toki-tabs/buffer-group))
      bufs
      collection)
  (dolist (b (buffer-list))
    (when (equal (toki-tabs/buffer-group b) group)
      (push b bufs)))
  bufs
  )

(buffer-local-value 'toki-tabs/buffer-group (get-buffer"*Completions*"))
(car (buffer-list))

(aref (buffer-name (get-buffer"*Completions*")) 0)

(project-current (get-buffer"*Completions*"))

(with-current-buffer "*Completions*"
  (setq toki-tabs/buffer-group
                              (funcall toki-tabs-project-root-function)))

;; 用 taxy 给 org-roam 的 tag 进行分类展示
(require 'taxy)
(use-package taxy-magit-section
  :load-path "site-lisp/taxy")

(defun buffery-major-mode (buffer)
  (buffer-local-value 'major-mode buffer))

(defvar buffery
  (make-taxy
   :name "Buffers"
   :taxys (list
           (make-taxy
            :name "Modes"
            :take (apply-partially #'taxy-take-keyed (list #'buffery-major-mode))))))

;; Note the use of `taxy-emptied' to avoid mutating the original taxy definition.
(taxy-plain
 (taxy-fill (buffer-list)
            (taxy-emptied buffery)))

;; 使用 make-taxy 来定义一个分类 (taxy)
(defvar numbery
  (make-taxy :name "Numbery"
	     :description "A silly taxonomy of numbers."
             :predicate #'(lambda (x) (> x 2))
             :then #'ignore
    ))

;; taxy-plain 用于渲染显示 taxy 的结果
;; taxy-fill 接受两个参数，第一个是我们输入的列表，第二个是我们定义的分类
;; 分类实际上就是作用到了每个列表中的每个元素上
(taxy-plain
 (taxy-fill (list 1 2 3 4) (taxy-emptied numbery)))

;; 使用 emacsql 去查找指定 tag 的 node
(org-roam-db-query [:select [title] :from tags :join nodes :on (= tags:node_id nodes:id) :where (= tag "双指针")])

(setq elfeed-show-entry-switch #'elfeed-display-buffer)

(defun elfeed-display-buffer (buf &optional act)
  (pop-to-buffer buf)
  (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

(defun elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
      (lambda (times)
        (interactive "p")
        (forward-line (* times (or 1 0)))
        (recenter)
        (call-interactively #'elfeed-search-show-entry)
        (select-window (previous-window))
        (unless elfeed-search-remain-on-entry (forward-line -1))))

  (define-key elfeed-search-mode-map (kbd "n") (elfeed-search-show-entry-pre +1))
  (define-key elfeed-search-mode-map (kbd "p") (elfeed-search-show-entry-pre -1))
  (define-key elfeed-search-mode-map (kbd "M-RET") (elfeed-search-show-entry-pre))

;; 在阅读 elfeed 的时候，将当前光标之前的 entry 都标记为已读
;; 直接选择区域从当前光标到最开始的位置，然后调用 `elfeed-search-selected' 获得
;; 区域中所有的 entry, 然后标记为已读
(defun elfeed-mark-read-before-cursor ()
  (interactive)
  (save-excursion
    ;; select region
    (set-mark (point))
    (goto-char (point-min))
    (let ((entries (elfeed-search-selected))
	  (buffer (current-buffer)))
      (cl-loop for entry in entries
	       do (elfeed-untag entry 'unread)
	       )
      ;; update display buffer
      (with-current-buffer buffer
	(mapc #'elfeed-search-update-entry entries))))
  (forward-line))

;; 设置这个参数，保证 ox-hugo 在导出带 id 的 link 时
;; 不会出现 unresolve 的问题
(require 'find-lisp)
(setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))

;; (setq ivy-dispatching-done-hydra-exit-keys '(("M-o" '(lambda () (pop hydra-stack)))
                                             ;; ("C-g" '(lambda () (pop hydra-stack)))))

;; 根据 roam tag 设置 hugo tag
(defun insert-hugo-tag-from-roam ()
  (interactive)

  (goto-char (point-min))

  ;; 得到当前 buffer/node 的 tags
  ;; 这里只是得到了 filetag, 参考 `org-roam-tag-add'
  (let ((tags (split-string (or (cadr (assoc "FILETAGS"
					     (org-collect-keywords '("filetags")))) "")
			    ":" 'omit-nulls))
	(match (re-search-forward "^#\\+HUGO_TAGS.+$" nil t)))
    (goto-char match)
    (message tags))
  )

;; 这个 slugify 函数有一些问题
;; 如果题目是 Two Sum II - Input... (167)
;; 会把 II 中间的两个空格给补成下划线
;; 而 lc 发送的请求里面是只有中间一个下划线的
(defun leetcode--slugify-title (title)
  "Make TITLE a slug title.
Such as 'Two Sum' will be converted to 'two-sum'."
  (let* ((str1 (replace-regexp-in-string "\s-\s" "-" (downcase title)))
	 (str2 (replace-regexp-in-string "\s+" "-" str1))
         (res (replace-regexp-in-string "[(),]" "" str2)))
    res))

(leetcode--slugify-title (plist-get (leetcode--get-problem-by-id 167) :title))
(plist-get (leetcode--get-problem-by-id 167) :title)

(leetcode--fetch-problem "two-sum-ii-input-array-is-sorted")

;; 用 org-ql 来做 org mode 文件的查询接口
(require 'org-ql)
(nth 0 (org-ql-query
  :select #'org-get-heading
  :from "~/Documents/org/gtd/next.org"
  :where '(todo "DOING")))

(require 'org-roam-ui)

;; self bootrap quelpa
;; (unless (package-installed-p 'quelpa)
  ;; (with-temp-buffer
    ;; (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    ;; (eval-buffer)
    ;; (quelpa-self-upgrade)))

;; (quelpa
 ;; '(quelpa-use-package
   ;; :fetcher git
   ;; :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)

;; eyebrowse 是保存每个 frame 的所有 window-configs
;; xs-eyebrowse-save, 保存当前 frame 的所有 window-configs
;; xs-eyebrowse-load, 加载一个保存的 window-configs, 或者某一个 slot

;; 将 chrome 的书签同步的 org 文件中

(defvar chrome-bookmarks-file
  (cl-find-if
   #'file-exists-p
   ;; Base on `helm-chrome-file'
   (list
    "~/Library/Application Support/Google/Chrome/Profile 1/Bookmarks"
    "~/Library/Application Support/Google/Chrome/Default/Bookmarks"
    "~/AppData/Local/Google/Chrome/User Data/Default/Bookmarks"
    "~/.config/google-chrome/Default/Bookmarks"
    "~/.config/chromium/Default/Bookmarks"
    (substitute-in-file-name
     "$LOCALAPPDATA/Google/Chrome/User Data/Default/Bookmarks")
    (substitute-in-file-name
     "$USERPROFILE/Local Settings/Application Data/Google/Chrome/User Data/Default/Bookmarks")))
  "Path to Google Chrome Bookmarks file (it's JSON).")

(defun chrome-bookmarks-insert-as-org ()
  "Insert Chrome Bookmarks as org-mode headings."
  (interactive)
  (require 'json)
  (require 'org)
  (let ((data (let ((json-object-type 'alist)
                    (json-array-type  'list)
                    (json-key-type    'symbol)
                    (json-false       nil)
                    (json-null        nil))
                (json-read-file chrome-bookmarks-file)))
        level)
    (cl-labels ((fn
                 (al)
                 (pcase (alist-get 'type al)
                   ("folder"
                    (insert
                     (format "%s %s\n"
                             (make-string level ?*)
                             (alist-get 'name al)))
                    (cl-incf level)
                    (mapc #'fn (alist-get 'children al))
                    (cl-decf level))
                   ("url"
                    (insert
                     (format "%s %s\n"
                             (make-string level ?*)
                             (org-make-link-string
                              (alist-get 'url al)
                              (alist-get 'name al))))))))
      (setq level 1)
      (fn (alist-get 'bookmark_bar (alist-get 'roots data)))
      (setq level 1)
      (fn (alist-get 'other (alist-get 'roots data))))))

(orb--new-note "Herzig-iclr-2021-object" '(:keys r))

(defun get-window-mode ()
  (let* ((windows (window-list-1))
	 (l '()))
    (dolist (w windows)
      (push (format "%s" (buffer-local-value 'major-mode (window-buffer w))) l))
    (message (mapconcat 'identity l ", "))))

;; (cancel-timer xs-org-clock-idle-watch-timer)
(setq print-idle-timer (run-with-timer 5 1 'get-window-mode))
(setq print-idle-timer (run-with-timer 5 1 '(lambda () (message "%s" (window-at-side-p (frame-first-window) 'right)))))
(setq print-idle-timer (run-with-timer 5 1 '(lambda () (message "%s" (window-in-direction 'right (frame-first-window))))))
(cancel-timer print-idle-timer)

;; 定义 worf 的 hydra
(use-package worf
  :pretty-hydra
  ((:title (pretty-hydra-title "worf move" 'faicon "wheelchair-alt" :height 1.1 :v-adjust -0.1 ) :color blue)
   ("Move"
    (("h" worf-left "left" :color red)
     ("j" worf-down "down" :color red)
     ("k" worf-up "up" :color red)
     ("l" worf-right "right" :color red)
     ("g" worf-goto "goto")
     ("[?\\t]" worf-tab "cycle" :color red))
    "refile"
    (("t" worf-refile-this "this" :color red)
     ("o" worf-refile-other "other" :color red)
     ("a" org-archive-subtree "archive" :color red))
    "Others"
    (("q" hydra-pop "exit")
     ("e" org-set-effort "effort" :color red)
     ("c" org-clock-in "clock in" :color red)
     ("M-l" nil)))))
(define-key org-mode-map (kbd "M-l") 'worf-hydra/body)

(defun xs-get-entry-title-marker ()
  (interactive)
  (let* ((heading (nth 4 (org-heading-components)))
	 (marker (xs-get-entry-marker))
	 (res (cons heading marker)))
    (with-output-to-temp-buffer "lxs"
      (print res))))

;; hugo blog tag
;; 由于我都是 file-based 的用法，所以只提取 file tag.
;; 如果已经有 hugo_tag 了，应该怎么处理？增量增加？
(defun xs-hugo-init-tag ()
  (interactive)
  "Set hugo tag as the same with roam node tag"
  (let* ((tags (split-string (or (cadr (assoc "FILETAGS"
					      (org-collect-keywords '("filetags")))) "") ":" 'omit-nulls)))
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward "#\\+HUGO_TAGS:")
      (dolist (tag tags)
	(insert (format " \"%s\"" tag))))))

;; All opened org buffers will be org refile targets
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 4)))
(setq org-refile-use-outline-path 'file)

(defun xs-org-log-delete ()
  (interactive)
  (save-excursion
    (goto-char (org-log-beginning))
      (org-mark-element)
      (delete-region (region-beginning) (region-end))
      (org-remove-empty-drawer-at (point))))

(defun swint-dired-rsync (action)
  (interactive)
  (let ((remote (completing-read "Remote repo: "
                                 (split-string
                                  (shell-command-to-string
                                   "cat ~/.ssh/config | grep \"^Host \" | awk '{print $2}'"))))
        (path (abbreviate-file-name default-directory))
        (is-push (equal action "push"))
        (is-pull (equal action "pull"))
        (string-to-escape "\\( \\|(\\|)\\|\\[\\|\\]\\|{\\|}\\)")
        rsync-command)
    ;; 对于rsync，escape本地路径用\，远程路径用\\\。
    (cl-flet ((escape-local (x)
                            (replace-regexp-in-string string-to-escape
                                                      "\\\\\\1" x))
              (escape-remote (x)
                             (replace-regexp-in-string string-to-escape
                                                       "\\\\\\\\\\\\\\1" x)))
      (let ((files (cond (is-push
                          (cl-loop for f in (dired-get-marked-files)
                                   collect (escape-local f)))
                         (is-pull
                          (let ((remote-files (helm-comp-read "Remote files: "
                                                              (split-string (shell-command-to-string
                                                                             ;; 连接remote列出path下文件绝对路径，并不显示错误信息。
                                                                             (format "proxychains4 ssh %s '(cd %s && ls -A | sed \"s:^:`pwd`/:\") 2>/dev/null'"
                                                                                     remote (escape-local path))) "\n")
                                                              :marked-candidates t
                                                              :buffer (format "*ivy rsync %s*" remote))))
                            (cl-loop for f in remote-files
                                     collect (concat remote ":" (escape-remote (if (directory-name-p f)
                                                                                   (directory-file-name f)
                                                                                 f))))))))
            (dest (cond (is-pull (escape-local path))
                        (is-push (escape-remote (concat remote ":" path))))))
        (setq rsync-command "proxychains4 rsync -arv --progress ")
        (dolist (file files)
          (setq rsync-command
                (concat rsync-command file " ")))
        (setq rsync-command (concat rsync-command dest))))
    (let ((process (start-process-shell-command "rsync" "*rsync*" rsync-command)))
      (lexical-let ((pos (memq 'mode-line-modes mode-line-format))
                    (mode-string action))
        (setcdr pos (cons (concat "Rsync/Unison " mode-string " ") (cdr pos)))
        (set-process-sentinel
         process
         (lambda (process signal)
           (when (memq (process-status process) '(exit signal))
             (message "Rsync/Unison %s done." mode-string)
             (setcdr pos (remove (concat "Rsync/Unison " mode-string " ") (cdr pos))))))))))

(defun swint-dired-rsync-push ()
  (interactive)
  (swint-dired-rsync "push"))

;TODO: disable hydra for this mode
(use-package dirvish
  :custom
  (dirvish-attributes '(vscode-icon file-size))
  ;; (dirvish-bookmarks-alist
   ;; '(("h" "~/"                          "Home")
     ;; ("d" "~/Downloads/"                "Downloads")
     ;; ("m" "/mnt/"                       "Drives")
     ;; ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  ;; In case you want the details at startup like `dired'
  ;; :hook
  ;; (dirvish-mode . (lambda () (dired-hide-details-mode -1)))
  :bind
  (:map dired-mode-map
        ("SPC" . dirvish-show-history)
        ("r"   . dirvish-roam)
        ("b"   . dirvish-goto-bookmark)
        ("f"   . dirvish-file-info-menu)
        ("M-a" . dirvish-mark-actions-menu)
        ("M-s" . dirvish-setup-menu)
        ("M-f" . dirvish-toggle-fullscreen)
        ([remap dired-summary] . dirvish-dispatch)
        ([remap dired-do-copy] . dirvish-yank)
        ([remap mode-line-other-buffer] . dirvish-other-buffer)))

(defun replace-id-quick ()
  (interactive)
  (let* ((num (completing-read "num:" nil)))
    (re-search-forward "\"id\": ")
    (save-excursion
      (delete-region (point) (progn (forward-word) (point))))
    (insert num)
    (other-window 1)
    (next-line)))

;; 固定一个侧边栏，这里面可以存放 file, roam node (org headlines or files), directory

(defun xs-org-sidebar-follow-mode ()
  (interactive)
  ;; 如果开启的话，每次打开 org mode buffer 的时候，都在侧边栏显示其 sidebar
  )

(use-package delve
  :load-path "site-lisp/delve"
  :bind
  ;; the main entry point, offering a list of all stored collections
  ;; and of all open Delve buffers:
  (("<f12>" . delve))
  :config
  ;; set meaningful tag names for the dashboard query
  (setq delve-dashboard-tags '("leetcode"))
 ;; turn on delve-minor-mode when org roam file is opened:
  (delve-global-minor-mode))

(use-package maple-explorer
  :load-path "site-lisp/emacs-maple-explorer"
  :commands (maple-explorer-file maple-explorer-buffer maple-explorer-imenu maple-explorer-recentf)
  :config
  (setq maple-explorer-file-display-alist '((side . left) (slot . -1)))
  (setq maple-explorer-buffer-display-alist '((side . left) (slot . -1)))
  (setq maple-explorer-recentf-display-alist '((side . left) (slot . -1))))

(defun maple-explorer-dashboard-list(&optional isroot)
  "Get recentf file list ISROOT mean first call."
  (list
   :name "dashboard"
   :value "dashboard"
   :face 'font-lock-constant-face
   :click 'maple-explorer-fold
   :status 'open
   :children
   (list
    (list :name "gtd"
          :face 'font-lock-keyword-face
          :click (lambda() (interactive) (message "bbb")))
    (list :name "bookmark"
          :face 'font-lock-keyword-face
          :click 'maple-explorer-fold
	  :status 'open
	  :children (list (maple-explorer-bm-list))
      ))))
(maple-explorer-define dashboard)

(defun bm-list ()
  (cl-loop
   for b in bookmark-alist
   for name = (car b)
   for location = (treemacs-canonical-path (bookmark-location b))
   collect (cons name location)))

(defun maple-explorer-bm-list ()
  (maple-explorer-list
   (bm-list)
   'maple-explorer-buffer-face
   'maple-explorer-bookmark-info))

(defun maple-explorer-bookmark-info (bm)
  (list :name (car bm)
	:face  'maple-explorer-buffer-item-face
	:click 'maple-explorer-bm-click
	:value (cdr bm)))

(defun maple-explorer-bm-click ()
  (interactive)
  (maple-explorer-with  (switch-to-buffer-other-window (find-file-noselect (plist-get info :value)))))

(bookmark-location "1-next")
(treemacs-canonical-path (bookmark-location "1-next"))

(use-package moldable-emacs
  ;; :init (if (f-directory-p "~/.emacs.d/site-lisp/moldable-emacs")
  ;;           (shell-command "cd ~/.emacs.d/site-lisp/moldable-emacs; git pull;")
  ;;         (shell-command "cd ~/.emacs.d/site-lisp/; git clone git@github.com:ag91/moldable-emacs.git"))
  :load-path "~/.emacs.d/site-lisp/moldable-emacs/"
  :bind (("C-c m m" . me-mold)
         ("C-c m f" . me-go-forward)
         ("C-c m b" . me-go-back)
         ("C-c m o" . me-open-at-point)
         ("C-c m d" . me-mold-docs)
         ("C-c m g" . me-goto-mold-source)
         ("C-c m e a" . me-mold-add-last-example)
         )
  :config
  (require 'moldable-emacs)
  ;; (add-to-list 'me-files-with-molds (concat (file-name-directory (symbol-file 'me-mold)) "molds/experiments.el")) ;; TODO this is relevant only if you have private molds
  (me-setup-molds))

(use-package "xeft"
  :load-path "site-lisp/xeft"
  :ensure nil
  :defer t
  :custom
  (xeft-directory "~/Documents/org/org-roam-files")
  :commands (xeft))

(defvar org-theme-css-dir "~/Documents/org/css")

(defun toggle-org-custom-inline-style ()
  (interactive)
  (let ((hook 'org-export-before-parsing-hook)
        (fun 'set-org-html-style))
    (if (memq fun (eval hook))
        (progn
          (remove-hook hook fun 'buffer-local)
          (message "Removed %s from %s" (symbol-name fun) (symbol-name hook)))
      (add-hook hook fun nil 'buffer-local)
      (message "Added %s to %s" (symbol-name fun) (symbol-name hook)))))

(defun org-theme ()
  (interactive)
  (let* ((cssdir org-theme-css-dir)
         (css-choices (directory-files cssdir nil ".css$"))
         (css (completing-read "theme: " css-choices nil t)))
    (concat cssdir css)))

(defun set-org-html-style (&optional backend)
  (interactive)
  (when (or (null backend) (eq backend 'html))
    (let ((f (or (and (boundp 'org-theme-css) org-theme-css) (org-theme))))
      (if (file-exists-p f)
          (progn
            (set (make-local-variable 'org-theme-css) f)
            (set (make-local-variable 'org-html-head)
                 (with-temp-buffer
                   (insert "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n")
                   (insert-file-contents f)
                   (goto-char (point-max))
                   (insert "\n/*]]>*/-->\n</style>\n")
                   (buffer-string)))
            (set (make-local-variable 'org-html-head-include-default-style)
                 nil)
            (message "Set custom style from %s" f))
        (message "Custom header file %s doesnt exist")))))
; 使用
(use-package recursive-search-references
  :load-path "site-lisp/recursive-search-references")

(defun xs/choose-insert-index-name ()
  "选择 roam 中 tag 为 index 的文件名，在导出 org 文件到 html 时，可能需要设置当前页面的上一级
页面，通常上一级页面是按相同 topic 维护的 org 笔记索引，这个函数弹出选择 index 页面，然后插入页面的文件名
到 org 文件的 file header."
  (interactive)
  (let* ((inds (mapcar #'car (org-roam-db-query [:select [file] :from tags :join nodes :on (= tags:node_id nodes:id) :where (= tag "index")])))
	 (names (mapcar #'file-name-base inds))
	 (name (completing-read "choose the index html:\n" names)))
    (insert (concat name ".html"))))

(use-package embark
  :ensure t
  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("M-n" . embark-next-symbol)
   ("M-p" . embark-previous-symbol)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(defun my-select-tab-by-name (tab)
  (interactive
   (list
    (let ((tab-list (or (mapcar (lambda (tab) (cdr (assq 'name tab)))
                                (tab-bar-tabs))
                        (user-error "No tabs found"))))
      (consult--read tab-list
                     :prompt "Tabs: "
                     :category 'tab))))
  (tab-bar-select-tab-by-name tab))

(embark-define-keymap embark-bib-actions
  "Keymap for actions for tab-bar tabs (when mentioned by name)."
  ("i" my/insert-bib-headline))

(defun my/insert-bib-headline ()
  (interactive)
  (let* ((entry (bibtex-completion-get-entry (car keys)))
	     (title (funcall orb-bibtex-entry-get-value-function "title" entry))
	     (key (funcall orb-bibtex-entry-get-value-function "key" entry)))
	(insert (concat title key))))

(defun test ()
  (interactive)
  (message (bibtex-completion-get-value "key" entry)))

(require 'worf)
(defun bjm/worf-insert-internal-link ()
  "Use ivy to insert a link to a heading in the current `org-mode' document. Code is based on `worf-goto'."
  (interactive)
  (let ((cands (worf--goto-candidates)))
    (ivy-read "Heading: " cands
              :action 'bjm/worf-insert-internal-link-action)))

(defun bjm/worf-insert-internal-link-action (x)
  "Insert link for `bjm/worf-insert-internal-link'"
  ;; go to heading
  (save-excursion
    (goto-char (cdr x))
    ;; store link
    (call-interactively 'org-store-link)
    )
  ;; return to original point and insert link
  (org-insert-last-stored-link 1)
  ;; org-insert-last-stored-link adds a newline so delete this
  (delete-backward-char 1)
  )

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'yasnippet)



(require 'corfu)
(require 'corfu-info)
(require 'corfu-history)
(require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
(corfu-history-mode t)
(global-lsp-bridge-mode)
(when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))  ;; adjust default font height when running in HiDPI screen.

;;; On Windows, commands run by flycheck may have CRs (\r\n line endings).
;;; Strip them out before parsing.
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
        )
    (funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/eaf-git")
(require 'eaf)
(require 'eaf-git)

(defun org-ask-location ()
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location "Headline" nil t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
    (end-of-line))

(setq org-capture-templates
 '(("l" "Log" entry
    (function org-ask-location)
    "\n\n** %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)))

(add-to-list 'org-capture-templates '("x" "test" plain (function (lambda () (find-file-noselect "~/Documents/org/gtd/next.org"))) "%U %i?"))

(start-process-shell-command "when-changed" "*lxs*" "when-changed")

(flymake-mode 1)

(setq org-id-extra-files (directory-files-recursively org-roam-directory "\.org$"))

(defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "C:/Windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
               nil
               nil))

(defun xs-insert-two-space ()
  (interactive)
  (insert "  ")
  (beginning-of-line (next-line)))

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 220))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(when (f-exists-p "~/plantuml.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (plantuml . t)
     ))
  (use-package plantuml-mode
    :config
    (setq org-plantuml-jar-path "~/plantuml.jar")
    ))

(shell-command-to-string "black")
(shell-command-to-string "wsl echo -e \"\\033[31;1;4m$(date)\\033[0m\n\"")
(eshell-command "C:\\Users\\xunsong.li\\AppData\\Roaming\\Python\\Python310\\Scripts\\black.exe")
(setenv "PATH"
	(concat
	 "C:\\Users\\xunsong.li\\AppData\\Roaming\\Python\\Python310\\Scripts\\" ";"
	 (getenv "PATH")))
(setq eshell-path-env (concat "C:\\Users\\xunsong.li\\AppData\\Roaming\\Python\\Python310\\Scripts\\" ";" eshell-path-env))


(defun tt-print-items ()
  "print all headings in current buffer (of org mode).
2019-01-14"
  (interactive)
  (with-output-to-temp-buffer "*xah temp out*"
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (x)
        (princ (org-element-property :raw-link x))
        (terpri )))))

(setq flycheck-flake8rc ".flake8")



(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")
(require 'lsp-bridge)
(add-hook 'prog-mode-hook #'lsp-bridge-mode)
;; (global-lsp-bridge-mode)

(define-key acm-mode-map "\M-j" #'hydra-reading/nil)
(define-key acm-mode-map "\M-k" #'pyim-convert-string-at-point)


(define-key prog-mode-map "\M-." #'lsp-bridge-find-def)
(define-key prog-mode-map "\M-," #'lsp-bridge-return-from-def)


;; add time display for rsync command
;; this command can run in cmd
;; wsl ~/.local/bin/when-changed -r -v -1 -s /mnt/c/users/xunsong.li/Bridge/ -c "sshpass -p \"Lxs1183236604@\" rsync -auvz --timeout=15 $(git rev-parse --show-toplevel) dev013:~/; echo \"\\033[31;1;4m$(date)\\033[0m\n\""


(defvar acm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] #'acm-select-next)
    (define-key map [remap previous-line] #'acm-select-prev)
    (define-key map [down] #'acm-select-next)
    (define-key map [up] #'acm-select-prev)
    (define-key map "\M-n" #'acm-select-next)
    (define-key map "\M-p" #'acm-select-prev)
    (define-key map "\M-," #'acm-select-last)
    (define-key map "\M-." #'acm-select-first)
    (define-key map "\C-m" #'acm-complete)
    (define-key map "\t" #'acm-complete)
    (define-key map "\n" #'acm-complete)
    (define-key map "\C-g" #'acm-hide)
    map)
  "Keymap used when popup is shown.")

(defun xs/org-ql-dblock-move-to-headline ()
  "Quick move to the headline as the line that the cursor being
in the dynamic table"
  (interactive)
  (goto-char (line-beginning-position))
  (let* (
	 (end (line-end-position))
	 (link-regex "\\[\\[.*+\\]\\[.*+\\]\\]")
	 (pos (- (re-search-forward link-regex end t) 1)))
    (goto-char pos)
    (org-open-at-point)))


;; 读取 node tag, 根据 tag 找对应的 index node name
(setq org-tag-to-index-node '(("python" "python 编程指南")))

(defun xs-insert-node-to-index-file ()
  (interactive)
  (let* ((node (org-roam-node-at-point))
	 (tags (org-roam-node-tags node))
	 )
    (dolist (tag tags)
      (if-let ((ind-file (xs--get-index-node-from-tag tag)))
	  ; TODO before insert, check if this ind-file has this node link
	  (xs--insert-node-into-file ind-file node)
	))))

(defun xs--get-index-node-from-tag (tag)
  (let* ((node-title (cadr (assoc tag org-tag-to-index-node)))
	 (ind-file (caar (org-roam-db-query [:select [file] :from nodes :where (= title $s1)] node-title)))
	 )
    ind-file))

(defun xs--insert-node-into-file (file node)
  (let* ((description (org-roam-node-formatted node))
	 (id (org-roam-node-id node))
	 )
    (with-current-buffer (find-file-noselect file)
      (end-of-buffer)
      (insert "- ")
      (insert (org-link-make-string
               (concat "id:" id)
               description)))))



(defun xs/lsp-bridge-not-in-comment ()
  "Hide completion if cursor in comment area."
  (interactive)
  (if (lsp-bridge-in-comment-p)
      (message "in comment")
    (message "no")
      ))

(use-package consult)

(setq treemacs-follow-mode nil
      )
(with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
(setq lsp-bridge-enable-signature-help nil)

;; 每次保存时，如果当前 buffer 在 sync 的 proj 里面，那么看对应的 buffer 长度是否发生了变化。需要注册 (sync-buffer-name, prev-length)
(defun syp-check-sync-state ()
  ()
  )
