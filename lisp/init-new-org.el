(require 'init-custom)
(require 'init-const)

(use-package org
  :ensure nil
  :commands (org-dynamic-block-define)
  :defer t
  :bind
  (("C-c a" . org-agenda)
   ("C-c b" . org-switchb)
   ("C-c c" . org-capture)
  (:map org-mode-map
  ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1))))
  ("C-c m" . hydra-org-movement/body)))
  :custom
  (org-src-block-faces 'nil)
  (org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)))
  (org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
     ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :init
  (setq lxs/org-agenda-directory (concat lxs-home-dir "Documents/org/gtd/"))
  ;; 这个 hook 似乎必须设置到 init 里面才能生效
  (add-hook 'org-mode-hook #'visual-line-mode)
  ;; I rewrite this function to fix the window pop behaviour of *Org todo*
  ;; Refer to this link: https://emacs.stackexchange.com/questions/14817/how-to-control-where-the-org-todo-keywords-buffer-displays
  ;; Maybe it is dangerours
  ;; (defun org-switch-to-buffer-other-window (args)
    "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer."
    ;; (switch-to-buffer-other-window args))
  :hook
  (((org-babel-after-execute org-mode) . org-redisplay-inline-images)
   (org-mode . org-hide-block-all)
   (org-mode . org-content)
   ;; (org-mode . toggle-word-wrap)
   ;; (org-mode . turn-on-org-cdlatex)
   ;; (org-mode . (lambda () (setq truncate-lines t)))
   ;; (org-mode . visual-line-mode)
   (org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist lxs-prettify-org-symbols-alist)
                       (prettify-symbols-mode 1))))
  ;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))

  :config
  ;; For hydra
  (defun hot-expand (str &optional mod)
    "Expand org template.
STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (setq org-ellipsis (if (and (display-graphic-p) (char-displayable-p ?⏷)) "\t⏷" nil)
	org-startup-indented t
	org-hide-emphasis-markers t
	org-catch-invisible-edits 'smart
	org-tags-column -77
	org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success)))

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))
  ;; 让 org 支持中文的格式标记
  (setq org-emphasis-regexp-components '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)

  ;; org-superstar-mode 美化 orgmode 的 headline
  (use-package org-superstar
    :ensure t
    :hook (org-mode . org-superstar-mode)
  :config
  (add-hook 'org-mode-hook #'org-superstar-mode))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (and (display-graphic-p) (char-displayable-p ?⯀))
                    '("⯀" "⯀" "⯀" "⯀")
                  '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

  (defhydra hydra-org-movement (:color red :columns 3)
    "Org Mode Movements"
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("g" org-goto "goto" :exit t))

  ;; 在 org-mode 中流畅地翻阅图片
  (use-package iscroll
    ;;
    :load-path "iscroll"
    :hook
    (org-mode . iscroll-mode)
    )

  (use-package valign
    :hook
    (org-mode . valign-mode))

  (use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    :config
    (;; (setq org-appear-autolinks t)
     ))

    ;; org habit
  (use-package org-tempo
    :ensure nil)
  (use-package org-habit
    :ensure nil)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t)
  (setq org-habit-graph-column 40)
  (setq org-habit-preceding-days 21)
  (setq org-habit-following-days 1)
  ;; org-protocol 设置
  (use-package org-protocol
    :ensure nil
    :config
    (setq org-protocol-default-template-key nil)
    (add-to-list 'org-modules 'org-protocol))

  ;; org tracking time
  '(org-clock-into-drawer "CLOCKING")

  ;; 设置 org-mode 显示图片大小
  (setq org-image-actual-width '(400))

  (setq org-support-shift-select t)

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

  ;; 补全 quote
  (defun org-completion-symbols ()
  (interactive)
  (when (looking-back "=[a-zA-Z\\-_]+")
    (let (cands)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "=\\([a-zA-Z\\-_]+\\)=" nil t)
            (cl-pushnew
             (match-string-no-properties 0) cands :test 'equal))
          cands))
      (when cands
        (list (match-beginning 0) (match-end 0) cands :exclusive 'no)))))

  (add-hook 'completion-at-point-functions
	    #'org-completion-symbols
	    'append)

  ;; org-capture 相关配置
  (defun get-year-and-month ()
    (list (format-time-string "%Y年") (format-time-string "%m月")))
  (defun find-month-tree ()
    "配合 journal 的 capture"
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

  (defun my/org-capture-maybe-create-id ()
    (when (org-capture-get :create-id)
      (org-id-get-create)))
  (add-hook 'org-capture-prepare-finalize-hook #'my/org-capture-maybe-create-id)
  (setq org-capture-templates
	`(("i" "待办" entry (file+headline ,(concat lxs/org-agenda-directory "next.org" "待办"))
           "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>")
	  ("c" "web bookmarks" entry (file ,(concat lxs/org-agenda-directory "webclips.org"))
	   "* [[%:link][%:description]]\n " :prepend t :empty-lines-after 1 :immediate-finish t)
	  ("n" "notes" entry (file+headline ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "quick-notes.org") "Notes")
	   "* %^{标题}\n%?" :create-id t)
	  ("s" "code cookbook" entry (file+headline ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "quick-notes.org") "Cookbook")
	   "* %^{描述}\n%i** 代码\n%?" :create-id t :jump-to-captured t)
	  ("a" "code api" entry (file+headline ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "quick-notes.org") "Api")
	   "* %?\n%i- Signature: ==\n描述: " :create-id t :jump-to-captured t)
	  ))
  (add-hook 'org-capture-before-finalize-hook #'org-set-created-property)

(defun lxs/org-find-project-journal-datetree ()
  ;; (interactive)
  (let* ((project (completing-read "Choose a project" '("compaction")))
	(m (org-find-olp (cons (org-capture-expand-file (concat lxs-home-dir "Documents/" "org/" "org-roam-files/" project ".org")) '("Journal")))))
    (set-buffer (marker-buffer m))
   ;; (org-capture-put-target-region-and-position)
   (widen)
   (goto-char m)
   (set-marker m nil)
   ;; (org-capture-put-target-region-and-position)
   (org-datetree-find-date-create (calendar-gregorian-from-absolute (org-today)) (when '("Journal") 'subtree-at-point))
   )
  )

(defun lxs/org-find-project-idea-datetree ()
  ;; (interactive)
  (let* ((project (completing-read "Choose a project" '("compaction")))
	(m (org-find-olp (cons (org-capture-expand-file (concat lxs-home-dir "Documents/" "org/" "org-roam-files/" project ".org")) '("Idea")))))
    (set-buffer (marker-buffer m))
   ;; (org-capture-put-target-region-and-position)
   (widen)
   (goto-char m)
   (set-marker m nil)
   ;; (org-capture-put-target-region-and-position)
   (org-datetree-find-date-create (calendar-gregorian-from-absolute (org-today)) (when '("Idea") 'subtree-at-point))
   )
  )

(add-to-list 'org-capture-templates `("w" "Project"))
(add-to-list 'org-capture-templates `("wj" "project joural" entry (function lxs/org-find-project-journal-datetree) "* %U - %^{heading}\n  %?"))
(add-to-list 'org-capture-templates `("wi" "project idea" entry (function lxs/org-find-project-idea-datetree)	"* %U - %^{heading}\n  %?"))

  ;; 为 org 的 header 的 created 和 last_modified 两个属性设置自动检测时间戳
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
  (defvar org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")
  
  (defun org-set-created-property (&optional active NAME)
  "Set a property on the entry giving the creation time.
By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
  (interactive)
  (let* ((created (or NAME org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now))))
  
  (defun zp/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (zp/org-set-time-file-property "LAST_MODIFIED")))
  (add-hook 'before-save-hook #'zp/org-set-last-modified)
  )

(defun lxs/org-show-recent-inserted-img ()
  (when (derived-mode-p 'org-mode)
    (call-interactively 'org-toggle-inline-images)
    (call-interactively 'org-toggle-inline-images)))
(add-hook 'before-save-hook #'lxs/org-show-recent-inserted-img)
;; save
(use-package org-pomodoro
  :after org-agenda
  :ensure t
  :bind
  (:map org-agenda-mode-map ("P" . org-pomodoro))
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
  (use-package alert-toast
    :load-path "alert-toast")
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq org-pomodoro-length 25)
  ;; (setq org-pomodoro-manual-break t)
  (setq org-pomodoro-long-break-frequency 3)
  (setq org-pomodoro-long-break-length 15))

;; org agenda and gtd setting
(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-files (directory-files-recursively lxs/org-agenda-directory "\\.org$")
	org-agenda-window-setup 'other-window)
  :defer t
  :bind
  (:map org-agenda-mode-map
   ("i" . org-agenda-clock-in)
   ("r" . lxs/org-process-inbox)
   ("R" . org-agenda-refile)
   ("c" . lxs/org-inbox-capture))
  :hook
  (after-init . org-agenda-mode)
  :config
  ;; 一些基础配置
  ;; (setq org-agenda-files (directory-files-recursively lxs/org-agenda-directory "\\.org$"))
  (add-to-list 'org-agenda-files (concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "paper_index.org"))
  (setq org-agenda-archives-mode t)
  ;; org-todo-list config
  (setq org-agenda-todo-list-sublevels nil)

  ;; 设置 TODO state and faces
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

  ;; 设置 agenda 显示位置
  ;; (setq org-agenda-window-setup 'current-window)
  ;; 设置 agenda 打开在行首而不是在最末尾
  ;; ref: https://www.reddit.com/r/orgmode/comments/j59h02/org_agenda_cursor_starts_at_bottom/
  ;; (add-hook 'org-agenda-finalize-hook #'org-agenda-find-same-or-today-or-agenda 90) ;; 这个 hook 会引发一些问题
  (add-hook 'org-agenda-finalize-hook (lambda () (goto-char (point-min))) 90)
  ;; 在 agenda 中按 Tab 打开 headline 在 底部弹出
  (define-advice org-agenda-goto (:around (orig-fn &rest args) "new-frame")
    (let ((display-buffer-overriding-action '(display-buffer-at-bottom)))
      (apply orig-fn args)))

  ;; agenda view
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
				                      ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "paper_index.org")))
                  ))
           (todo "TODO"
		         ((org-agenda-overriding-header "Projects")
                  (org-agenda-files '(,(concat lxs/org-agenda-directory "projects.org")
				                      ,(concat lxs/org-agenda-directory "learning.org")
				                      ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "paper_index.org")))
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
		          (org-agenda-files '(,(concat lxs-home-dir "Documents/" "org/" "HugoBlogs/" "short-notes.org")))))
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

  ;; 快速切换到 agenda view
  (defun lxs/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  (bind-key "<f5>" 'lxs/switch-to-agenda)
  ;; 设置默认的 tag
  (setq org-tag-alist (quote (("@home" . ?h)
                              ("@school" . ?s)
                              (:newline)
                              ("WAITING" . ?w)
                              ("HOLD" . ?H)
                              ("CANCELLED" . ?c))))

  ;; 设置 refile 的目标文件夹 all file from inbox.org to these
  (setq org-refile-use-outline-path 'file
	    org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("next.org" :level . 1)
                             ("someday.org" :level . 1)
                             ("reading.org" :level . 1)
                             ("projects.org" :maxlevel . 2)
			                 ("learning.org" :level . 1)))

  ;; 在任务 clock in 后，将其从 TODO 状态切换到 NEXT 状态
  (defun lxs/set-todo-state-next ()
    "Visit each parent task and change NEXT states to TODO"
    (org-todo "NEXT"))
  (add-hook 'org-clock-in-hook 'lxs/set-todo-state-next 'append)

  ;; 批量处理 inbox.org 中的文件
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
     ;; (org-agenda-set-tags)
     (org-agenda-priority)
     (call-interactively 'lxs/my-org-agenda-set-effort)
     (org-agenda-refile nil nil t)))

  (defvar lxs/org-agenda-bulk-process-key ?f
    "Default key for bulk processing inbox items.")

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

  (defun lxs/org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture nil "i"))  ;; 存疑，似乎没有用到过

  ;; archive done and cancelled tasks
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

  )

(use-package ox-latex
  :defer t
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

(use-package ox-hugo
  :ensure t
  :custom
  (org-hugo-auto-set-lastmode t)
  (org-hugo-section "post")
  :after ox)

(use-package easy-hugo
:init
(setq easy-hugo-basedir (concat lxs-home-dir "Documents/" "xssq-blog/"))
(setq easy-hugo-url "https://patrolli.github.io/xssq/")
(setq easy-hugo-root "/docs")
(setq easy-hugo-postdir "content/post")
(setq easy-hugo-previewtime "300"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat lxs-home-dir "Documents/" "org/" "org-roam-files"))
  (org-roam-completion-everywhere t)
  :bind ("C-c n" . org-roam-hydra/body)
  :pretty-hydra
  ((:title (pretty-hydra-title "Org roam menu" 'faicon "book"  :height 1.1 :v-adjust -0.1)
    :color blue)
   ("Basic"
    (("f" org-roam-node-find "find nodes")
     ;; ("b" org-roam-switch-to-buffer "switch buffer")
     ("i" org-roam-node-insert "insert")
     ("u" my/roam-init-node "init node")
     ("p" org-toggle-properties "show proper")
     ("t" org-roam-tag-add "add tag"))
    "Dailies"
    (("j" org-roam-dailies-capture-today "capture today")
     ("y" org-roam-dailies-capture-yesterday "capture yesterday")
     ("J" org-roam-dailies-goto-today "goto today")
     ("Y" org-roam-dailies-goto-yesterday "goto yesterday")
     ("d" org-roam-dailies-goto-date "goto date"))
    "Others"
    (("v t" org-tags-view "filt buffer tags")
     ("a" org-roam-buffer-toggle "backlinks")
     ("s" org-clock-in "clock in")
     ("c" org-clock-out "clock out")
     ("q" hydra-pop "exit"))))
  :config
  (org-roam-setup)
  (setq org-roam-node-display-template "${my-tags}${filetitle}${olp}${title:*}")

  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (let ((filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
          (title (org-roam-node-title node)))
      (if (string= filetitle title)
          ""
        (format "%s > " filetitle))))

  (cl-defmethod org-roam-node-my-tags ((node org-roam-node))
    (let ((tags (org-roam-node-tags node)))
      (if (> (length tags) 0)
	  (format "(%s) " (mapconcat (lambda (s) (concat "" s)) tags ","))
	"")))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${slug}.org"
            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_alias: \n#+STARTUP: inlineimages latexpreview\n#+AUTHOR:Li Xunsong\n")
           :unnarrowed t)
          ("p" "private" plain
           "%?"
           :file-name "private-${slug}.org"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
	  ("s" "code snippet"
	   "* description\n* code\n#+BEGIN_SRC %^{language}\n%^C%?\n#+END_SRC\n* note\n* ref\n"
	   :file-name "snippet-${slug}.org"
	   :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+AUTHOR: Li Xunsong\n")
	  ("r" "ref" plain "%?"
	   :if-new (file+head "${citekey}.org" "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_ALIAS: \n#+AUTHOR: Li Xunsong\n#+DATE: %<%Y-%m-%d>\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+STARTUP: inlineimages latexpreview hideblocks\n\n* Motivation\n\n* Method\n\n* Comment\n\n* Ref\n")
	   :unnarrowed t)))

	;; 设置 org-roam-dailies
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates (let ((head
          (concat
           "#+title: %<%A, %d %B %Y>\n#+filetags: :private: :dailies:\n* Log\n* Review\n")))
     `(("r" "review" entry
        "* %?"
        :if-new (file+head+olp "%<%Y>/%<%B>/%<%Y-%m-%d>.org" ,head ("Review"))
        :unnarrowed t)
       ("l" "log" entry
        "* %U: %?"
        :if-new (file+head+olp "%<%Y>/%<%B>/%<%Y-%m-%d>.org" ,head ("Log")))
       )))

  (require 'org-roam-protocol)
  (defun my/roam-init-node ()
    "init org-roam headline node"
    (interactive)
    (progn (org-id-get-create)
           (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))
  )

(use-package org-roam-bibtex
  :config
  (org-roam-bibtex-mode))

(use-package org-ref
  :ensure t
  :after org
  :init
  ;; (setq org-ref-bibtex-hydra-key-binding "\C-cj")
  :config
  (setq reftex-default-bibliography '("/mnt/c/Users/lixun/Documents/bibliography/library.bib"))
  (use-package helm-bibtex
    :ensure t
    :after org-ref
  :config
  (setq bibtex-completion-bibliography (concat lxs-home-dir "Documents/" "bibliography/" "library.bib")))
  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "/mnt/c/Users/lixun/Documents/org/paper-reading.org"
	org-ref-default-bibliography '("/mnt/c/Users/lixun/Documents/bibliography/library.bib")
	org-ref-pdf-directory "/mnt/c/Users/lixun/Documents/bibliography")
  (global-set-key (kbd "C-c ]") 'org-ref-helm-insert-cite-link)
  (global-set-key (kbd "C-c s") 'dblp-lookup)
  )

(use-package org-crypt
  :defer t
  :config
  (require 'epa-file)
  (epa-file-enable)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  )

;; export org to docx
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) "-org-export" ".docx"))
           (template-file "/mnt/c/Users/lixun/Documents/org/Summaries/template.docx"))
    (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (message "Convert finish: %s" docx-file)))

;; Babel
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
(require 'ob-ipython)
(defvar load-language-list '((emacs-lisp . t)
			     (perl . t)
			     (python . t)
			     (ipython . t)
			     (ruby . t)
			     (js . t)
			     (css . t)
			     (sass . t)
			     (C . t)
			     (plantuml . t)))

(org-babel-do-load-languages 'org-babel-load-languages
			     load-language-list)

(use-package org-clock-watch
  ;; 由于系统的原因，似乎不能弹出通知
  :disabled t
  :load-path "~/.emacs.d/site-lisp/org-clock-watch" ;; TODO: 不能使用相对路径吗？
  :config
  (org-clock-watch-toggle 'on)
  (setq org-clock-watch-work-plan-file-path (concat lxs/org-agenda-directory "next.org"))
  (setq org-show-notification-handler 'message)
  )

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

(server-start)

(provide 'init-new-org)
