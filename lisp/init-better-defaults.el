;; (electric-indent-mode 1)
;;set open recent files

;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-max-menu-item 10)


(fset 'yes-or-no-p 'y-or-n-p)


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


(use-package watch-other-window
  :load-path "watch-other-window/"
  :bind
  (:map prog-mode-map
	("M-n" . watch-other-window-up-line)
	("M-p" . watch-other-window-down-line)
	("M-o n" . watch-other-window-up)
	("M-o p" . watch-other-window-down)
	)
  )

;; 跳转光标
(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(setq mark-ring-max 3)
(setq global-mark-ring-max 6)

;; 使用 color-rg 来对 org 文件进行检索
(defun lxs/search-org ()
  (interactive)
  (color-rg-search-input (color-rg-read-input) (concat lxs-home-dir "Documents/" "org/"))
  )

;; 使用 windows 的程序来打开文件
;; 将这个命令绑定到 dired mode 的 C-c C-o
;; 可以用来打开 pdf, word, ppt 等
(defun xah-open-in-xternal-app-from-wsl()
  "open desktop by send command from wsl into powershell"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (directory (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" default-directory))
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         )
    (shell-command (concat powershell " -command \"start " (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" (nth 0 -file-list)) "\""))
    )
  )

;; can also enter C-c M-b Tab to quickly fold code block
(defun my/org-code-block-fold ()
  (interactive)
  (call-interactively 'org-previous-block)
  (org-cycle))

(provide 'init-better-defaults)
