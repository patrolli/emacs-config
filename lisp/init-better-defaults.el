(electric-indent-mode 1)
;;set open recent files
(setq make-makeup-files nil)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; forbid autosaving and backup
(setq make-backup-files nil)
(setq auto-save-default nil)
(set-face-attribute 'default nil :height 140)

;; indent buffer 
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indent selected region."))
      (progn
	(indent-buffer)
	(message "Indent buffer.")))))

;; expand the company 
(setq hippie-expand-try-function-list '(try-expand-debbrev
					try-expand-debbrev-all-buffers
					try-expand-debbrev-from-kill
					try-complete-file-name-partially
					try-complete-file-name
					try-expand-all-abbrevs
					try-expand-list
					try-expand-line
					try-complete-lisp-symbol-partially
					try-complete-lisp-symbol))

(fset 'yes-or-no-p 'y-or-n-p)

(setq dired-recursive-copies 'always)

;; 配置dired，所有目录共用一个buffer
(put 'dired-find-alternate-file 'disabled nil)
;; 延迟加载dired，节省emacs启动时间
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
;; 打开当前目录的dired mode，快捷键C－c C－j
(require 'dired-x)
(setq dired-dwin-target 1)

;; 在elisp模式下，取消单引号的自动配对
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)

;;高亮括号配对
;; (define-advice show-paren-function (:around (fn) fix-show-paren-function)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn))
;; 	(t (save-excursion
;; 	     (ignore-errors (backward-up-list))
;; 	     (funcall fn)))))
(show-paren-mode nil)
;; 仅针对 elisp 文件显示括号配对
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 加载指定路径下的所有 .org 文件
(defun my/load-org () (interactive)
 (progn  (mapc (lambda (file-name) (find-file-noselect file-name))
         (directory-files-recursively "/mnt/c/Users/lixun/Documents/org/" "\.org$"))
	 (message "Finish loading.")) "")


(defun my/eprint ()
  "Download PDF according to ArXiv ID or URL"
  (interactive)
  (let* ((entry (progn (bibtex-beginning-of-entry)
                       (bibtex-parse-entry)))
         (arxiv (substring (assoc-default  "eprint" entry) 1 -1))
         (url (if (string-match "http" arxiv) arxiv (format "https://arxiv.org/pdf/%s.pdf" arxiv)))
         (key (assoc-default  "=key=" entry))
         (dpath (expand-file-name "./"))
         (fpath (format "%s/%s.pdf" dpath key)))
    (start-process-shell-command ""  nil (format "curl %s > %s" url fpath))
    (message "download %s to %s" url fpath)))

(defun my/cite-parse ()
  (let* ((parsed-cite (org-ref-parse-cite))
         (cite-key (nth 0 (car parsed-cite)))
         (cite-end-pos (nth 2 (car parsed-cite)))
         (line-end-pos (line-end-position))
         (have-title (if (< cite-end-pos line-end-pos) t nil)))
    (list cite-key have-title cite-end-pos line-end-pos)))

(defun my/cite-title (key)
  (substring
   (with-temp-buffer
     (insert
      (assoc-default
         "title"
	 (with-temp-buffer
	   (insert (org-ref-get-bibtex-entry key))
	   (bibtex-beginning-of-entry)
	   (bibtex-parse-entry)
	   )))
     (let ((fill-column 100000))
       (fill-paragraph nil))
     (buffer-string)
     )
   1 -1))

(defun my/add-title ()
  ""
  (interactive)
  (re-search-forward "cite:")
  (let* ((parsed-cite (my/cite-parse))
         (have-title (nth 1 parsed-cite))
         (cite-key (nth 0 parsed-cite)))
    (if have-title
        (progn (next-line)
	       (beginning-of-line))
      (let ((title (my/cite-title cite-key)))
        (end-of-line)
        (insert " ")
        (insert title)
        (next-line)
	(beginning-of-line)
	))))

;; (setq debug-on-error t)
(defun my/ref-category-set ()
  (interactive)
  (let ((key (org-entry-get (point) "Custom_ID")))
    (save-excursion
      (with-current-buffer
	(find-file "/mnt/c/Users/lixun/Documents/org/paper-taxonomy.org")
	(goto-char (point-min))
	(setq my-paper-note-category (if (search-forward key nil t)
					 (nth 4 (org-heading-components))
				       "Not Classify Yet"))
	)
      (switch-to-prev-buffer)
      (org-entry-put (point) "TAXONOMY" (capitalize my-paper-note-category))
      (outline-back-to-heading)
      (org-set-tags   (replace-regexp-in-string "[:\s:-]" "" (capitalize my-paper-note-category)))
      ;; (org-set-tags ":t:")
      )))

(global-auto-revert-mode t)

;; TODO: 递归地 sort 
(defun my/org-sort-by-prio ()
  (interactive)
  (org-sort-entries t ?p)
  (org-sort-entries t ?o))
;; emacs gc 的优化
;; (defvar k-gc-timer
;;   (run-with-idle-timer 15 t
;;                        'garbage-collect))

;; (defmacro k-time (&rest body)
;;   "Measure and return the time it takes evaluating BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (float-time (time-since time))))

;; (defvar k-gc-timer
;;   (run-with-idle-timer 15 t
;;                        (lambda ()
;;                          (message "Garbage Collector has run for %.06fsec"
;;                                   (k-time (garbage-collect))))))


(defun my/helm-org-run-marked-heading-id-link ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action
        'my/helm-org-marked-heading-id-link)))

(defun my/helm-org-marked-heading-id-link (marker)
     (let* ((victims (with-helm-buffer (helm-marked-candidates)))
            (buffer (marker-buffer marker))
            (filename (buffer-file-name buffer))
            (rfloc (list nil filename nil marker)))
       (when (and (= 1 (length victims))
                  (equal (helm-get-selection) (car victims)))
         ;; No candidates are marked; we are refiling the entry at point
         ;; to the selected heading
         (setq victims (list marker)))
       (when (and victims buffer filename rfloc)
         (cl-loop for victim in victims
                  ;; do (org-with-point-at victim
                  ;;      (org-refile nil nil rfloc))

                  do (with-current-buffer (marker-buffer victim)
         (let ((heading-id (save-excursion (goto-char (marker-position victim))
                                           (org-id-get-create)
                                           ))
               (heading-name
                (save-excursion
                  (goto-char (marker-position victim))
                  (org-entry-get nil "ITEM"))
                )
               )
           (with-helm-current-buffer
             (org-insert-link
              nil (concat "id:" heading-id) heading-name)
             (insert " ")
             )))
   ))))

(add-to-list 'helm-org-headings-actions '("Insert id link(s) C-c h l" . my/helm-org-marked-heading-id-link) t)
;; (add-to-list 'helm-org-rifle-actions '("Insert id link(s) C-c h l" . my/helm-org-marked-heading-id-link) t)


(setq focus-follows-mouse t)

(require 'watch-other-window)
(bind-key "M-n" #'watch-other-window-up-line)
(bind-key "M-p" #'watch-other-window-down-line)
(bind-key "M-o n" #'watch-other-window-up)
(bind-key "M-o p" #'watch-other-window-down)

;; 跳转光标
(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
(global-set-key (kbd "<f6>") 'xah-pop-local-mark-ring)
(global-set-key (kbd "<f7>") 'pop-global-mark)

(provide 'init-better-defaults)
