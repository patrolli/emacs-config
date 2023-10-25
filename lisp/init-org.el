(require 'init-custom)
(require 'init-const)

;; (use-package alert-toast
  ;; :load-path "alert-toast")

(use-package org
  :ensure nil
  :commands (org-dynamic-block-define)
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
  ("C-c m" . hydra-org-movement/body)
  ("C-c ]" . nil)))
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
  ;; hook 似乎必须设置到 init 里面才能生效
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'(lambda ()
			       "Beautify org symbols."
			       (setq prettify-symbols-alist lxs-prettify-org-symbols-alist)
			       (unless prettify-symbols-mode
				 (prettify-symbols-mode 1))))
  :hook
  ((org-mode . org-hide-block-all)
   (org-mode . org-content))
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
                             (?C . success))
	org-log-done t
	org-clock-x11idle-program-name "xprintidle")

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

  (use-package valign
    :hook
    (org-mode . valign-mode))

  (use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    :config
    (;; (setq org-appear-autolinks t)
     ))

  (use-package xenops
    :ensure t
    :hook
    (org-mode . (lambda () (xenops-mode -1))))

    ;; org habit
  (use-package org-tempo
    :ensure nil)
  (use-package org-habit
    :ensure nil)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t
	org-habit-graph-column 40
	org-habit-preceding-days 21
	org-habit-following-days 1)
  
  ;; org-protocol 设置
  (use-package org-protocol
    :ensure nil
    :config
    (setq org-protocol-default-template-key nil)
    (add-to-list 'org-modules 'org-protocol))

  ;; org tracking time
  (setq org-clock-into-drawer "CLOCKING")

  ;; 设置 org-mode 显示图片大小
  (setq org-image-actual-width '(400))

  (setq org-support-shift-select t)

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

  ;; 补全 quote
  (defun org-completion-symbols ()
  (interactive)
  (when (looking-back "=[[:ascii:]]+$")
    (let (cands)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "=\\([[:ascii:]]+\\)=$" nil t)
            (cl-pushnew
             (match-string-no-properties 0) cands :test 'equal))
          cands))
      (when cands
        (list (match-beginning 0) (match-end 0) cands :exclusive 'no)))))

  (defun my-org-register-completion-functions-h ()
      (add-hook 'completion-at-point-functions #'org-completion-symbols -100 t))
  (add-hook 'org-mode-hook #'my-org-register-completion-functions-h)

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
  (defun my/org-capture-maybe-prev-create-id ()
    (when (org-capture-get :create-prev-id)
      (outline-previous-heading)
      (outline-previous-heading)
      (org-id-get-create)))
  (add-hook 'org-capture-prepare-finalize-hook #'my/org-capture-maybe-create-id)
  (add-hook 'org-capture-prepare-finalize-hook #'my/org-capture-maybe-prev-create-id)
  (setq org-capture-templates
	`(("i" "待办" entry (file+headline ,(concat lxs/org-agenda-directory "next.org") "待办")
           "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>")
	  ("h" "之后" entry (file+headline ,(concat lxs/org-agenda-directory "next.org") "Inbox")
	   "* %?\nCaptured %<%Y-%m-%d %H:%M>"
	   )
	  ("c" "web bookmarks" entry (file ,(concat lxs/org-agenda-directory "webclips.org"))
	   "* [[%:link][%:description]]\n " :prepend t :empty-lines-after 1 :immediate-finish t)
	  ("n" "notes" entry (file+headline ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "quick-notes.org") "Notes")
	   "* %^{标题}\n%?" :create-id t)
	  ("s" "code cookbook" entry (file+headline ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "quick-notes.org") "Cookbook")
	   "\n* %^{描述}\n%?" :create-prev-id t :jump-to-captured t)
	  ("a" "code api" entry (file+headline ,(concat lxs-home-dir "Documents/" "org/" "org-roam-files/" "quick-notes.org") "Api")
	   "* %?\n%i- Signature: ==\n描述: " :create-id t :jump-to-captured t)
	  ))
  (add-hook 'org-capture-before-finalize-hook #'org-set-created-property)

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
   ("R" . org-agenda-refile))
  ;; :hook
  ;; (after-init . org-agenda-mode)
  :config
  ;; 一些基础配置
  ;; (setq org-agenda-files (directory-files-recursively lxs/org-agenda-directory "\\.org$"))

  (setq org-agenda-archives-mode t)
  ;; org-todo-list config
  (setq org-agenda-todo-list-sublevels nil)

  ;; 设置 TODO state and faces
  (setq org-todo-keywords
	    '((sequence "TODO(t)" "DOING(n)" "|" "DONE(d)")
          (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-todo-keyword-faces
	    (quote (("TODO" :foreground "red" :weight bold)
		        ("DOING" :inherit warning)
		        ("DONE" :foreground "forest green" :weight bold)
		        ("HOLD" :foreground "magenta" :weight bold)
		        ("CANCELLED" :foreground "forest green" :weight bold)
		        ("REPEAT" :foreground "red" :weight bold)
		        )))

  ;; refile
  (setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

  ;; 在任务 clock in 后，将其从 TODO 状态切换到 DOING 状态
  (defun lxs/set-todo-state-next ()
    "Visit each parent task and change DOING states to TODO"
    (org-todo "DOING"))
  (add-hook 'org-clock-in-hook 'lxs/set-todo-state-next 'append)

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
  :ensure nil
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
  (org-roam-directory (concat lxs-home-dir "Documents/" "org/" "org-roam-files/"))
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
    "Timer"
    (("s" (org-clock-in '(4)) "clock recent")
     ("S" org-clock-in "clock in")
     ("c" org-clock-out "clock out")
     ("g" org-clock-goto "clock goto")
     )
    "Others"
    (("v t" org-tags-view "filt buffer tags")
     ("a" org-roam-buffer-toggle "backlinks")
     ("q" hydra-pop "exit"))))
  :config
  (org-roam-setup)
  ;; (setq org-roam-node-display-template "${my-tags}${filetitle}${title:*}")
  (setq org-roam-node-display-template "${my-tags}${title:80}")

  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (let ((filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
          (title (org-roam-node-title node)))
      (if (string= filetitle title)
          ""
        (format "%s > " filetitle))))

  (cl-defmethod org-roam-node-my-tags ((node org-roam-node))
    (let ((tags (org-roam-node-tags node)))
      (propertize
       (if (> (length tags) 0)
           (format "(%s) " (mapconcat (lambda (s) (concat "" s)) tags ","))
	 "")
       'font-lock-face '(:foreground "grey"))))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${slug}.org"
            "#+title: ${title}\n#+filetags: :refile:\n#+date: %<%Y-%m-%d>\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+roam_alias: \n#+startup: inlineimages latexpreview\n#+author: xunsong\n")
           :unnarrowed t)
          ("p" "private" plain
           "%?"
           :file-name "private-${slug}.org"
           :head "#+title: ${title}\n"
           :unnarrowed t)
	  ("r" "paper notes" plain "%?"
	   :if-new (file+head "${citekey}.org" "#+title: ${title}\n#+filetags: :refile:\n#+author: xunsong\n#+date: %<%Y-%m-%d>\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+startup: inlineimages\n\n")
	   :unnarrowed t)))

	;; 设置 org-roam-dailies
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
     `(("l" "log" plain
        "- [%<%H:%M>]\n- %?"
        :target (file+head+olp "%<%Y>/%<week_%V>.org" "#+title: %<week %V %B %Y>\n#+filetags: :private:dailies:" ("%<%m-%d, %a>"))
        :unnarrowed t)))

  (require 'org-roam-protocol)
  ;; 设置 org-protocol 的 catpure 模板
  (setq org-roam-capture-ref-templates
	'(
	  ("r" "ref" plain "%?"
	   :target (file+head "${slug}.org"
			      "#+title: ${title}")
	   :unnarrowed t)
	  ("l" "leetcode" plain "%?"
	   :if-new (file+head "${slug}.org"
			      "#+title: ${title}\n#+filetags: :leetcode:refile:\n\n* Solution\n\n")
	   :unnarrowed t)))

  (defun my/roam-init-node ()
    "init org-roam headline node"
    (interactive)
    (progn (org-id-get-create)
           (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))

  (defun my/orb-init-node ()
    "init org-roam headline with REFS: for org-roam-bibxtex"
    (interactive)
    ;; 首先检查 citekey 是否已经对应了一个 roam node
    ;; 需要根据 headline 的标题进行查找
    )
  )

(use-package org-roam-bibtex
  :after (org-roam)
  ;; :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (org-roam-bibtex-mode))

(use-package org-ref
  :ensure t
  :init
  ;; (setq org-ref-bibtex-hydra-key-binding "\C-cj")
  :config
  (setq reftex-default-bibliography `(,(concat lxs-home-dir "Documents/bibliography/library.bib")))
  (require 'org-ref-ivy)
  (with-eval-after-load "org"
    (global-set-key (kbd "C-c ]") 'org-ref-insert-link)
    (global-set-key (kbd "C-c s") 'dblp-lookup)
    )
  )

(use-package ivy-bibtex
    :ensure t
    :config
  ;; bibtex-completions settings
  (setq bibtex-completion-bibliography `(,(concat lxs-home-dir "Documents/" "bibliography/" "library.bib")))
  (setq bibtex-completion-library-path `(,(concat lxs-home-dir "Documents/" "bibliography/")))

  (ivy-set-actions
   'ivy-bibtex
   '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI" ivy-bibtex-open-any)
     ("e" ivy-bibtex-edit-notes "Edit notes" ivy-bibtex-edit-notes)
     ("c" ivy-bibtex-create-headline "Create headline" ivy-bibtex-create-headline)
     ("i" ivy-bibtex-insert-headline "Insert headline")
     ))
    ;; 定制一个 roam insert node 的 function.
    ;; 供 `bibtex-completion-create-roam-headline' 函数使用
    (defun my-orb-node-insert (node)
      (let* ((description (org-roam-node-title node)))
	(insert (org-link-make-string
                 (concat "id:" (org-roam-node-id node))
                 description)))
      )
    (defun bibtex-completion-create-roam-headline (keys)
      ;; 这个函数是针对 ivy-bibtex 插入 ref 时使用
      (if-let ((node (orb-note-exists-p (car keys))))
	  ;; 这里确认有这个 node, 我们直接插入 roam 的 node link
	  (my-orb-node-insert node)
	(message "no roam keys, insert cite links")
	(insert (format "cite:%s" (car keys)))
	)
      )
    ;;
    (defun bibtex-completion-insert-headline (keys)
      (let* ((key (car keys))
	     (venue (nth 1 (split-string key "-")))
	     (year (substring (nth 2 (split-string key "-")) 2))
	     (entry (bibtex-completion-get-entry key))
	     (title (funcall orb-bibtex-entry-get-value-function "title" entry)))
	(insert (format "%s (%s'%s)" title venue year))
	)
      )
    (ivy-bibtex-ivify-action bibtex-completion-create-roam-headline ivy-bibtex-create-headline)
    (ivy-bibtex-ivify-action bibtex-completion-insert-headline ivy-bibtex-insert-headline))

;; 在 bibtex mode 下一些有用的函数
;; (use-package bibtex-utils
;;   :load-path "site-lisp/bibtex-utils")

;; export org to docx
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) "-org-export" ".docx"))
        (template-file "/home/lixunsong/Documents/org/Summaries/template.docx"))
    ;; (message (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (message "Convert finish: %s" docx-file)))

;; export org to html
(require 'ox-publish)
(require 'org-roam-export)
(setq org-publish-project-alist
      '(
       ("org-notes"
	:base-directory "~/Documents/org/org-roam-files/"
	:base-extension "org"
	:publishing-directory "~/Documents/org/publish_html/"
	:recursive t
	:publishing-function org-html-publish-to-html
	:headline-levels 4             ; Just the default for this project.
	:auto-preamble t
	:auto-sitemap t
	:sitemap-filename  "sitemap.org"   ; ... 称它为 sitemap.org（它是默认的）... 
	:sitemap-title  "Sitemap"          ; ...标题为“站点地图”。
	)
       ("org-static"
	:base-directory "~/Documents/org/static/"
	:base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	:publishing-directory "~/public_html/"
	:recursive t
	:publishing-function org-publish-attachment
	)
       ("org" :components ("org-notes" "org-static"))))

(setq org-export-use-babel nil)
(setq org-export-with-broken-links 'mark)
(setq org-html-htmlize-output-type 'css)
(setq org-html-head-include-default-style nil)

(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-publish-current-file after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-publish-current-file t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-publish-current-file nil t)
    (message "Enabled org html export on save for current buffer...")))

;; Babel
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(use-package ob-ipython
  :ensure t)

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

(use-package org-download
  :ensure t
  :config
  (org-download-enable)
  (setq-default org-download-image-dir (concat lxs-home-dir "Documents/" "org/" "static/" "img/"))
  (setq org-download-screenshot-method "xfce4-screenshooter -r -o cat > %s")
  (defun org-download--dir-2 ()
    (file-name-base (buffer-file-name))))

;; 将 org buffer 中的图片复制到剪贴板
;; [[file:../static/img/emacs_hacks/2022-01-16_23-25-08_screenshot.png]]
(defun xs-org-img-to-clipboard-at-point ()
  (interactive)
  (let* ((dir-path (org-download--dir))
	 (current-name (file-name-nondirectory (org-element-property :path (org-element-context))))
	 (img-path (concat dir-path "/" current-name)))
    (call-process-shell-command (format "cat %s | xclip -selection clipboard -target image/png -i" img-path) nil nil)))

(defun xs-org-img-to-clipboard-at-line ()
  (interactive)
  (let* ((line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	 (dir-path (org-download--dir))	 
	 (file-path (when (string-match "\\[\\[file:\\(.+\\)\\]\\]" line-str)
		      (match-string 1 line-str)))
	 (current-name (file-name-nondirectory file-path))
	 (img-path (concat dir-path "/" current-name)))
    (message img-path)
    (call-process-shell-command (format "cat %s | xclip -selection clipboard -target image/png -i" img-path) nil nil)))

;TODO: 向前和向后查找
(defun xs-toggle-code-block ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (org-in-src-block-p)
	(progn
	  (re-search-backward "#\\+begin_" nil t 1)
	  (org-hide-block-toggle))
      ;TODO: goto block
      (re-search-forward "#\\+begin_" nil t 1)
      (org-hide-block-toggle))))

(defun xs-next-code-block ()
  (interactive)
  (end-of-line)
  (unless (re-search-forward "#\\+begin_" nil t 1)
    (re-search-forward "#\\+end_" nil t 1)
    (next-line)))

(defun xs-prev-code-block ()
  (interactive)
  (beginning-of-line)
  (re-search-backward "#\\+begin_" nil t 1))

;; save edicted org files every one hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; org-sidebar
(use-package org-sidebar
  :config
  (setq org-sidebar-tree-jump-fn #'org-sidebar-tree-jump-source))


(cl-defun org-sidebar-tree-jump-source (&key children)
  "Jump to the heading at point in its source buffer.
If CHILDREN is non-nil (interactively, with prefix), also expand
child entries.  Should be called from a tree-view buffer."
  (interactive "P")
  (unless (buffer-base-buffer)
    (error "Must be in a tree buffer"))
  (let* ((pos (point))
         (base-buffer (buffer-base-buffer))
         (window (get-buffer-window base-buffer)))
    (if window
        (progn
          (select-window window)
          (switch-to-buffer base-buffer))
      (pop-to-buffer base-buffer
                     (cons 'display-buffer-use-some-window
                           (list (cons 'inhibit-same-window t)))))
    (widen)
    (goto-char pos)
    (org-show-entry)
    (org-show-children)
    (when children
      (org-show-subtree))
    (org-narrow-to-subtree)))

(defun xs-toggle-gtd-sidebar ()
  (interactive)
  (let* ((buf (find-file-noselect "~/Documents/org/gtd/next.org")))
    (with-current-buffer buf
      (if (buffer-narrowed-p)
	    (narrow-to-region 1 (1+ (buffer-size))))
      (switch-to-buffer buf)
      (org-sidebar-tree-toggle))))
(global-set-key (kbd "<f5>") #'xs-toggle-gtd-sidebar)

(server-start)

(use-package denote
  :ensure t
  :config
  (setq denote-directory (file-name-concat lxs-home-dir "Documents" "org" "denotes")
	denote-file-type 'markdown-yaml))

(use-package denote-menu
  :ensure t)

(provide 'init-org)
