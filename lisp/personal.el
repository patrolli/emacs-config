(require 'init-const)

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
     (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun lxs/open-compaction-file ()
  (interactive)
  (let ((my-compaction-file "/mnt/c/Users/lixun/Documents/org/org-roam-files/compaction.org"))
    (find-file my-compaction-file)))

;; (global-set-key (kbd "<f3>") #'lxs/open-compaction-file)

(defun lxs/org-find-datetree ()
  (interactive)
  (let ( (m (org-find-olp (cons (org-capture-expand-file "~/test.org") '("test"))) ))
    (set-buffer (marker-buffer m))
   (org-capture-put-target-region-and-position)
   (widen)
   (goto-char m)
   (set-marker m nil)
   (org-capture-put-target-region-and-position)
   (org-datetree-find-date-create (calendar-gregorian-from-absolute (org-today)) (when '("test") 'subtree-at-point))
   )
  )

(defun lxs-org-is-hugo-file-p (fPath)
  "Predict if the org file has been converted into hugo"
  (with-temp-buffer
    (let ((keyline "#+HUGO_DRAFT: false\n"))
	  (insert-file-contents fPath)
	  (and (search-forward keyline nil t) t)
	  )
    ))

;; (defun lxs-org-is-hugo-file-p-test ()
  ;; "Predict if the org file has been converted into hugo"
  ;; (interactive)
  ;; (with-temp-buffer
    ;; (let ((keyline "#+HUGO_DRAFT: false\n"))
	  ;; (insert-file-contents "/mnt/c/Users/lixun/Documents/org/org-roam-files/emacs_counsel_find_file_快速输入目标路径.org")
	  ;; (if (and (search-forward keyline nil t) t)
	      ;; (message "yes")
	    ;; (message "no"))
	  ;; )
    ;; ))

(setq lxs-org-hugo-header-info "#+HUGO_BASE_DIR: ~/Documents/xssq-blog\n#+HUGO_AUTO_SET_LASTMOD: t\n#+HUGO_TAGS: \n#+HUGO_CATEGORIES: \n#+HUGO_DRAFT: false\n\n")

(defun lxs/org-hugo-export-current-buffer ()
  (interactive)
  (if (lxs-org-is-hugo-file-p (buffer-name))
      (org-hugo--export-file-to-md (buffer-name))
    (progn
      (beginning-of-buffer)
      (re-search-forward ":END:\n")
      (insert lxs-org-hugo-header-info)
      (org-hugo--export-file-to-md (buffer-name)
      ))
  ;; (org-hugo--export-file-to-md (buffer-name))
    ))

;; 运行 hugo 文件夹下面的 deploy.sh, 发布博客
;; 把 easy-hugo 的 `easy-hugo-github-deploy' 拿过来改的
(defun lxs/deploy-hugo ()
  (interactive)
  (let* ((hugo-url "https://patrolli.github.io/xssq/")
	 (github-deploy-script "deploy.sh")
	 (hugo-basedir "~/Documents/xssq-blog/")
	 (default-directory hugo-basedir)
	 (deployscript (file-truename (expand-file-name
				       github-deploy-script
				       hugo-basedir))))
    (print deployscript)
    (unless (executable-find deployscript)
      (error "%s do not execute" deployscript))
    (let ((ret (call-process-shell-command deployscript nil "*hugo-github-deploy*" t)))
       (unless (zerop ret)
	 (switch-to-buffer (get-buffer "*hugo-github-deploy*"))
	 (error "%s command does not end normally" deployscript)))
     (when (get-buffer "*hugo-github-deploy*")
       (kill-buffer "*hugo-github-deploy*"))
     (message "Blog deployed")
     (when hugo-url
       (browse-url hugo-url))))

;; call windows exe to open file
(defmacro wsl--open-with (id &optional app dir)
  `(defun ,(intern (format "wsl/%s" id)) ()
     (interactive)
     (wsl-open-with ,app ,dir)))

(defun wsl-open-with (&optional app-name path)
  "Send PATH to APP-NAME on WSL."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "%s `wslpath -w \"%s\"`" (shell-quote-argument app-name) path)))
    (shell-command-to-string command)))

;; (format "%s `wslpath -w %s`" (shell-quote-argument "explorer.exe") (shell-quote-argument (buffer-file-name)))

;; ( shell-command-to-string (format "%s `wslpath -w \"%s\"`" (shell-quote-argument "explorer.exe") "/mnt/c/Users/lixun/Documents/Calibre/Jeffrey E.F.Friedl/Jing Tong Zheng Ze Biao Da Shi _Di 3Ban (80)/Jing Tong Zheng Ze Biao Da Shi _Di 3Ban - Jeffrey E.F.Friedl.epub"))

(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
(wsl--open-with reveal-in-explorer "explorer.exe" default-directory)
(wsl--open-with open-in-vscode "Code.exe" buffer-file-name)

(defun reveal-in-explorer ()
  (interactive)
  (if sys/linuxp
      (shell-command (format "thunar %s" default-directory))
    ;; In windows, we need to convert / into \\
    (shell-command-to-string (format "explorer.exe %s"  (subst-char-in-string ?/ ?\\ default-directory)))))

(defun browse-html-of-org-buffer ()
  (interactive)
  (let* ((html-dir-path (file-name-concat (file-truename "~") "Documents" "org" "publish_html"))
	 (bn (file-name-base (buffer-name (current-buffer))))
	 (html-path (file-name-concat html-dir-path (concat bn ".html"))))
    (if (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
	(org-publish-current-file))
    (browse-url-of-file html-path)))

(defun my-delete-whole-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (progn (beginning-of-line) (point))
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(defun my-kill-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-counsel-grep-or-swiper (&optional initial-input)
  (interactive)
  (if (not buffer-file-name)
      (swiper initial-input)
    (counsel-grep initial-input)
    )
  )

;; quick insert timestamp in texts
(defvar current-date-time-format "%F %a %H:%M"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-date-format "%a %b %d"
  "Format of date to insert with `insert-curent-date' func")

(defun lxs/insert-current-date-time-with-line-split ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (let () (or comment-start "")))
  (insert "==========\n")
  (insert (let () (or comment-start "")))
  ;; (insert " ")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun lxs/insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (let () (or comment-start "")))
  (insert (concat "[" (format-time-string current-date-time-format (current-time))) "]"))

(defun lxs/insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n"))

(defun lxs/insert-current-date ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-date-format (current-time)))
       (insert "\n"))

(defun xs-set-bookmark (&optional name)
  (interactive)
  (let ((filename (or name (format "%s:%s" (buffer-name) (line-number-at-pos)))))
    (bookmark-set filename)))

(provide 'personal)

