;; config for org-agenda
;; i modify the path, in order to sync the org documents by jianguo
;; (setq org-agenda-files '("/mnt/c/Users/lixun/Documents/org/tdd.org" "/mnt/c/Users/lixun/Documents/org/inbox.org"))
(setq org-agenda-files '("/mnt/c/Users/lixun/Documents/org/"))

(require 'org-tempo)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines)))
(setq org-capture-templates
      `(("t" "task" entry (file "/mnt/c/Users/lixun/Documents/org/inbox.org")
         "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>") 
        ("n" "note" entry (file "/mnt/c/Users/lixun/Documents/org/notes.org")
         "* Note %<%Y-%m-%d %H:%M>\n%?")
      ("b" "web bookmarks" entry (file "/mnt/c/Users/lixun/Documents/org/web-clip.org")
       "* [[%:link%?][%:description]]\n")))

(setq org-agenda-custom-commands
      (quote (("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (tags "REFILE"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'scheduled))
                    (org-agenda-overriding-header "Global list of Un-scheduled tasks:")
                    ))))

          ("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          )))

(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
 (todo . " %i %-12:c")
 (tags . " %i %-12:c")
 (search . " %i %-12:c")))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE"'agenda))

;; TODO states define
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ;; ("NEXT" :foreground "blue" :weight bold)
	      ("NEXT" :inherit warning)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
	      ("REPEAT" :foreground "red" :weight bold)
	      ;; ("PROJECT" :inherit font-lock-string-face)
	      ("PROJECT" :foreground "red" :weight bold)
	      )))
;; 事件触发，状态转换设定
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
;; C-c C-c key 快速切换 todo 状态
(setq org-use-fast-todo-selection t)
;; S-left S-right 切换 todo 状态
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; 将 refile 文件移动到 org-agenda-file 下面
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				(org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; latex 工具
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

;; org tracking time
'(org-clock-into-drawer "CLOCKING")

;; org-todo-list config
(setq org-agenda-todo-list-sublevels nil)

;; org-ref
;; org-ref
(require 'org-ref)
(setq reftex-default-bibliography '("/mnt/c/Users/lixun/Documents/bibliography/library.bib"))
;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "/mnt/c/Users/lixun/Documents/org/paper-reading.org"
      org-ref-default-bibliography '("/mnt/c/Users/lixun/Documents/bibliography/library.bib")
      org-ref-pdf-directory "/mnt/c/Users/lixun/Documents/bibliography")

(setq org-support-shift-select t)

(setq org-image-actual-width nil)
(setq org-image-actual-width (/ (display-pixel-width) 3))

(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))
  (setq org-pomodoro-length 25)
  ;; (setq org-pomodoro-manual-break t)
  (setq org-pomodoro-long-break-frequency 2)
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
  :ensure t)

;; 还不太会用
(use-package org-journal
  :ensure t
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  ;; (setq org-journal-dir ("/mnt/c/Users/lixun/Documents/org/personal-journal" "/mnt/c/Users/lixun/Documents/org/research-journal"))
  (setq org-journal-dir "/mnt/c/Users/lixun/Documents/org/journal")
  )



(provide 'init-org)
