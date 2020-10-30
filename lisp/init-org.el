
;; config for org-agenda
;; i modify the path, in order to sync the org documents by jianguo
(setq org-agenda-files '("/mnt/c/Users/lixun/Documents/org"))
(require 'org-tempo)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines)))

(setq org-capture-templates
      `(("t" "task" entry (file "/mnt/c/Users/lixun/Documents/org/inbox.org")
         "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>") 
        ("n" "note" entry (file "/mnt/c/Users/lixun/Documents/org/notes.org")
         "* Note %<%Y-%m-%d %H:%M>\n%?")))

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

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE"'agenda))

;; TODO states define
(setq org-todo-keywords
      (quote ((sequence "TODO(t)"  "|" "PROJ(p)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" ))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
	      ("REPEAT" :foreground "red" :weight bold)
	      ("PROJ" :foreground "red" :weight bold)
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


(provide 'init-org)
