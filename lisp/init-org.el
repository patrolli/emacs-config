;; config for org-agenda
;; i modify the path, in order to sync the org documents by jianguo
;; (setq org-agenda-files '("/mnt/c/Users/lixun/Documents/org/tdd.org" "/mnt/c/Users/lixun/Documents/org/inbox.org"))
(use-package org
    :defer t)
(require 'org-agenda)
(server-start)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-protocol)
  )
(require 'org-protocol)
(require 'find-lisp)
(setq lxs/org-agenda-directory (concat lxs/home-dir "Documents/org/gtd/"))
(setq org-agenda-files
      (find-lisp-find-files lxs/org-agenda-directory "\.org$"))
(add-to-list 'org-agenda-files (concat lxs/home-dir "Documents/" "org/" "org-roam-files/" "paper_index.org"))

(setq org-agenda-archives-mode t)

(require 'org-tempo)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines)))

(defun get-year-and-month ()
  (list (format-time-string "%Y年") (format-time-string "%m月")))


(defun find-month-tree ()
  (let* ((path (get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min))             ;移动到 buffer 的开始位置
    ;; 先定位表示年份的 headline，再定位表示月份的 headline
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading)))
            (cnt 0))
        (if (re-search-forward re end t)
            (goto-char (point-at-bol))  ;如果找到了 headline 就移动到对应的位置
          (progn                        ;否则就新建一个 headline
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))

(setq org-capture-templates
      `(("i" "task" entry (file ,(concat lxs/org-agenda-directory "inbox.org"))
         "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>")
	("p" "capture paper" entry (file ,(concat lxs/org-agenda-directory "inbox.org"))
	 "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
        ("n" "note" entry (file ,(concat lxs/home-dir "Documents/" "org/" "notes.org"))
         "* %^{heading}\n%?")
      ("c" "web bookmarks" entry (file ,(concat lxs/org-agenda-directory "webclips.org"))
       "* [[%:link][%:description]]\n " :prepend t :empty-lines-after 1 :immediate-finish t)
      ("j" "journal" entry (file+datetree ,(concat lxs/home-dir "Documents/" "org/" "journal.org")) 
       "* %U - %^{heading}\n  %?")
      ("b" "billing" plain
               (file+function ,(concat lxs/home-dir "Documents/" "org/" "billing.org") find-month-tree)
               " | %U | %^{类别|吃饭|日用|其他} | %^{描述} | %^{金额} |" :kill-buffer t)
      ("s" "code snippet" entry (file ,(concat lxs/home-dir "Documents/" "org/" "snippet.org"))
       "* %<%Y-%m-%d> - %^{title}\t%^g\n#+BEGIN_SRC %^{language}\n%^C%?\n#+END_SRC")
      ("h" "hugo blog file" entry (file lxs/creat-hugo-file) "* ")
))

(setq org-protocol-default-template-key nil)

(defun lxs/org-agenda-prefix-string ()
(let ((path (org-format-outline-path (org-get-outline-path))))
  (if (> (length path) 0)
	   (concat "[" path "]") "")
       ))
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
 (todo . " %i %-12:c %(lxs/org-agenda-prefix-string)")
 (tags . " %i %-12:c")
 (search . " %i %-12:c")))
(setq lxs/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 14)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(concat lxs/org-agenda-directory "inbox.org")))))
         (todo "NEXT|HOLD"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(concat lxs/org-agenda-directory "someday.org")
                                    ,(concat lxs/org-agenda-directory "projects.org")
                                    ,(concat lxs/org-agenda-directory "next.org")
                                    ,(concat lxs/org-agenda-directory "learning.org")
				    ,(concat lxs/home-dir "Documents/" "org/" "org-roam-files/" "paper_index.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(concat lxs/org-agenda-directory "projects.org")
				    ,(concat lxs/org-agenda-directory "learning.org")
				  ,(concat lxs/home-dir "Documents/" "org/" "org-roam-files/" "paper_index.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '(,(concat lxs/org-agenda-directory "next.org")
				    ,(concat lxs/org-agenda-directory "someday.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	 (todo "TODO|NEXT"
	       ((org-agenda-overriding-header "Reading")
		(org-agenda-files '(,(concat lxs/org-agenda-directory "reading.org")))
		))
	 (todo "TODO"
	       ((org-agenda-overriding-header "Blogs")
		(org-agenda-files '(,(concat lxs/home-dir "Documents/" "org/" "HugoBlogs/" "short-notes.org")))))
	 (todo "WAITING"
	       ((org-agenda-overriding-header "Waiting List")
		(org-agenda-files '(,(concat lxs/org-agenda-directory "next.org")
				    ,(concat lxs/org-agenda-directory "someday.org")
				    ,(concat lxs/org-agenda-directory "projects.org")
				    ,(concat lxs/org-agenda-directory "learning.org")))))
         nil)))


(add-to-list 'org-agenda-custom-commands
             `("r" "Reading" todo ""
               ((org-agenda-files '(,(concat lxs/org-agenda-directory "reading.org"))))))
(add-to-list 'org-agenda-custom-commands `,lxs/org-agenda-todo-view)

;; 设置 agenda 显示位置
(setq org-agenda-window-setup 'current-window)

(defun lxs/switch-to-agenda ()
  (interactive)
  (org-agenda nil " "))

(bind-key "<f5>" 'lxs/switch-to-agenda)

(defun lxs/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'lxs/set-todo-state-next 'append)

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE"'agenda))

(defun org-archive-cancelled-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED"'agenda))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :inherit warning)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
	      ("REPEAT" :foreground "red" :weight bold)
	      )))


;; ;; 事件触发，状态转换设定
;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("HOLD" ("WAITING") ("HOLD" . t))
;;               (done ("WAITING") ("HOLD"))
;;               ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
;; C-c C-c key 快速切换 todo 状态
(setq org-use-fast-todo-selection t)
;; S-left S-right 切换 todo 状态
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; all todos captured in inbox.org and then refiled to target files
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("next.org" :level . 1)
                           ("someday.org" :level . 1)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 2)
			   ("learning.org" :level . 1)))

(setq org-tag-alist (quote (("@home" . ?h)
                            ("@school" . ?s)
                            (:newline)
                            ("WAITING" . ?w)
                            ("HOLD" . ?H)
                            ("CANCELLED" . ?c))))

;; refer to https://emacs.stackexchange.com/questions/59657/how-to-bulk-mark-agenda-items-based-on-file-name
;; a modification version of org-agenda-bulk-mark-regexp to match entry based on category
(defun custom/org-agenda-bulk-mark-regexp-category (regexp)
    "Mark entries whose category matches REGEXP for future agenda bulk action."
    (interactive "sMark entries with category matching regexp: ")
    (let ((entries-marked 0) txt-at-point)
      (save-excursion
        (goto-char (point-min))
        (goto-char (next-single-property-change (point) 'org-hd-marker))
        (while (and (re-search-forward regexp nil t)
                    (setq category-at-point
                          (get-text-property (match-beginning 0) 'org-category)))
          (if (get-char-property (point) 'invisible)
              (beginning-of-line 2)
            (when (string-match-p regexp category-at-point)
              (setq entries-marked (1+ entries-marked))
              (call-interactively 'org-agenda-bulk-mark)))))
      (unless entries-marked
        (message "No entry matching this regexp."))))

(defun lxs/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (custom/org-agenda-bulk-mark-regexp-category "inbox")
  (lxs/bulk-process-entries))

(defun lxs/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'lxs/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

(defvar lxs/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

;; 批量处理 org agenda 中的 entry
(defun lxs/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'lxs/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(setq org-agenda-bulk-custom-functions `((,lxs/org-agenda-bulk-process-key lxs/org-agenda-process-inbox-item)))

(defun lxs/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(defvar lxs/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun lxs/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " lxs/org-current-effort) nil nil lxs/org-current-effort)))
  (setq lxs/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil lxs/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
(define-key org-agenda-mode-map "r" 'lxs/org-process-inbox)
(define-key org-agenda-mode-map "R" 'org-agenda-refile)
(define-key org-agenda-mode-map "c" 'lxs/org-inbox-capture)


;; latex 工具
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

;; org tracking time
'(org-clock-into-drawer "CLOCKING")

;; org-todo-list config
(setq org-agenda-todo-list-sublevels nil)

;; org-ref
(require 'org-ref)
(setq reftex-default-bibliography '("/mnt/c/Users/lixun/Documents/bibliography/library.bib"))
;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "/mnt/c/Users/lixun/Documents/org/paper-reading.org"
      org-ref-default-bibliography '("/mnt/c/Users/lixun/Documents/bibliography/library.bib")
      org-ref-pdf-directory "/mnt/c/Users/lixun/Documents/bibliography")

(setq org-support-shift-select t)

;; 设置 org-mode 显示图片大小
(setq org-image-actual-width t)
;; (setq org-image-actual-width (/ (display-pixel-width) 3))


(require 'alert-toast)
(use-package org-pomodoro
  :ensure t
  :hook
  ((org-pomodoro-finished . (lambda ()
                (alert-toast-notify '(:title "pomodoro" :message "Task finished !!!" :data (:long t)))
                ))
   (org-pomodoro-short-break-finished . (lambda ()
                (alert-toast-notify '(:title "pomodoro" :message "A short break done, ready a new pomodoro !!!" :data (:long t)))
                ))
   (org-pomodoro-long-break-finished . (lambda ()
					 (alert-toast-notify '(:title "pomodoro" :message "A long break done, ready a new pomodoro !!!" :data (:long t)))
                )))
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))
  (setq org-pomodoro-length 25)
  ;; (setq org-pomodoro-manual-break t)
  (setq org-pomodoro-long-break-frequency 3)
  (setq org-pomodoro-long-break-length 15))

;; org habit
(setq org-habit-show-all-today t)
(setq org-habit-graph-column 40)
(setq org-habit-preceding-days 21)
(setq org-habit-following-days 1)

(use-package ox-latex
  :config
  (add-to-list 'org-latex-classes
           '("note"
         "
% default
\\documentclass[colorlinks]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{upgreek}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage{hyperref}
\\usepackage[cache=false]{minted}

% additional
\\usepackage{geometry}    % page size and margins
\\usepackage{fontspec}    % inline Chinese
\\usepackage{xeCJK}
\\usepackage{parskip}     % blank lines between paragraphs
\\usepackage{color}       % background of inline code
\\usepackage{xcolor}
\\usepackage{lscape}      % scaling and rotating of tables and images 
\\usepackage{adjustbox}

[NO-DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]
\\geometry{a4paper,scale=0.8}            % page margins
\\XeTeXlinebreaklocale \"zh\"            % break line with \"zh\" mode
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.36}                       % line spread 
\\setlength{\\parindent}{0cm}            % indent
\\definecolor{light-gray}{gray}{0.89}
\\renewcommand{\\texttt}[1]{{\\colorbox{light-gray}{\\small\\menlo #1}}}
"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; (setq org-latex-default-class "ctexart")
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process '("xelatex -shell-escape -interaction=nonstopmode -8bit -output-directory %o %f"
                        "bibtex %b"
                        "xelatex -shell-escape -interaction=nonstopmode -8bit -output-directory %o %f"
                        "xelatex -shell-escape -interaction=nonstopmode -8bit -output-directory %o %f"))
  )

;; export org to docx
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) "-org-export" ".docx"))
           (template-file "/mnt/c/Users/lixun/Documents/org/Summaries/template.docx"))
    (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (message "Convert finish: %s" docx-file)))

(use-package ox-hugo
  :ensure t
  :after ox)

;; 还不太会用
;; (use-package org-journal
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq org-journal-prefix-key "C-c j")
;;   :config
;;   ;; (setq org-journal-dir ("/mnt/c/Users/lixun/Documents/org/personal-journal" "/mnt/c/Users/lixun/Documents/org/research-journal"))
;;   (setq org-journal-dir "/mnt/c/Users/lixun/Documents/org/journal")
;;   )

;;--------------------------
  ;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
  ;;--------------------------

  (defun zp/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun zp/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (zp/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

  (defun zp/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (zp/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun zp/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (zp/org-set-time-file-property "LAST_MODIFIED")))


(use-package org-roam
  :ensure t
  :defer t
  :hook
  (after-init . org-roam-mode)
  :custom
      (org-roam-directory "/mnt/c/Users/lixun/Documents/org/org-roam-files")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph)
	       ("C-c n b" . org-roam-switch-to-buffer)
	       ("C-c n t" . org-roam-tag-add)
	       ("C-c n j" . org-roam-dailies-capture-today)
	       ("C-c n a" . org-roam-dailies-today)
	       ("C-c n y" . org-roam-dailies-yesterday))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
      :config
      (setq org-roam-verbose nil)
      (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_alias: \n#+roam_tags:\n#+STARTUP: inlineimages latexpreview\n#+AUTHOR:Li Xunsong\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
	  ("s" "code snippet" plain (function org-roam--capture-get-point)
	   "* code\n#+BEGIN_SRC %^{language}\n%^C%?\n#+END_SRC\n* description\n"
	   :file-name "snippet-${slug}"
	   :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_tags: \"code snippet\"\n#+AUTHOR: Li Xunsong\n")))
      (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}\n#+roam_tags: websites\n
- source :: ${ref}"
           :unnarrowed t)))
	(add-to-list 'org-roam-capture-ref-templates
             '("a" "Annotation" plain (function org-roam-capture--get-point)
               "%U\n ${body}\n"
               :file-name "${slug}"
               :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n#+roam_tags: websites\n"
               :immediate-finish t
               :unnarrowed t))
	;; 设置 org-roam-dailies
	(setq org-roam-dailies-directory "daily/")
	(setq org-roam-dailies-capture-templates
	      '( ;; ("d" "default" entry
		 ;;  #'org-roam-capture--get-point
		 ;;  "* %?"
		 ;;  :file-name "daily/%<%Y-%m-%d>"
		 ;;  :head "#+title: %<%Y-%m-%d>\n\n* 工作 \n\n* 备忘 \n\n* 随笔 \n\n* 总结 \n\n")
		("w" "work" entry
		 #'org-roam-capture--get-point
		 "* %<[%H:%M:%S]> - %?"
		 :file-name "daily/%<%Y-%m-%d>"
		 :head "#+TITLE: Journal %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: private journal\n\n"
		 :olp ("工作"))
		("n" "notes" entry
		 #'org-roam-capture--get-point
		 "* %<[%H:%M:%S]> - %?"
		 :file-name "daily/%<%Y-%m-%d>"
		 :head "#+TITLE: Journal %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: private journal\n\n"
		 :olp ("备忘"))
		("j" "journal" entry
		 #'org-roam-capture--get-point
		 "* %<[%H:%M:%S]> - %?"
		 :file-name "daily/%<%Y-%m-%d>"
		 :head "#+TITLE: Journal %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: private journal\n\n"
		 :olp ("随笔"))
		("r" "review" entry
		 #'org-roam-capture--get-point
		 "* %<[%H:%M:%S]> - %?"
		 :file-name "daily/%<%Y-%m-%d>"
		 :head "#+TITLE: Journal %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: private journal\n\n"
		 :olp ("总结"))))
	)

(add-hook 'before-save-hook #'zp/org-set-last-modified)

(require 'org-roam-protocol)
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 9090
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography (concat lxs/home-dir "Documents/" "bibliography/" "library.bib")))

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-insert-interface 'helm-bibtex))

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${citekey}"
         :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: \n#+ROAM_ALIAS: \n#+AUTHOR: Li Xunsong\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+STARTUP: inlineimages latexpreview hideblocks\n\n* Motivation\n\n* Method\n\n* Comment\n\n* Ref\n" 
         :unnarrowed t)))

;; 这个 package 有一些问题
;; 即时渲染 latex 代码
;; (use-package org-latex-impatient
;;   :ensure t
;;   :defer t
;;   :hook (org-mode . org-latex-impatient-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin
;;         ;; location of tex2svg executable
;;         "/mnt/c/Users/lixun/node_modules/mathjax-node-cli/bin/tex2svg")
;;   :config
;; )

;; 让 org 支持中文的格式标记
(setq org-emphasis-regexp-components '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(org-element-update-syntax)

;; 在 org-mode 中流畅地翻阅图片
(require 'iscroll)
(add-hook 'org-mode-hook #'iscroll-mode)

(define-key org-mode-map (kbd "C-c C-q") 'counsel-org-tag)

;; org-superstar-mode 美化 orgmode 的 headline
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-superstar-mode))

(define-advice org-agenda-goto (:around (orig-fn &rest args) "new-frame")
  (let ((display-buffer-overriding-action '(display-buffer-at-bottom)))
    (apply orig-fn args)))

(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;; 默认打开 org 文件，不显示代码块
(add-hook 'org-mode-hook 'org-hide-block-all)
(add-hook 'org-mode-hook 'org-overview)

(provide 'init-org)
