;; scratch buffer 的代码有用的一定要记得保存，不然就找不回来了

(defun lxs/org-refile-headline-by-tag ()
  (interactive)
  (save-excursion
  (let ((tags (org-get-tags nil t))
	(headline  (nth 4 (org-heading-components)))
	remove-flag)
    ;; if the headline has no tag and being under "Un-archieve"
    (when (and (eq 0 (length tags))  (progn
				       (save-excursion
				       (outline-up-heading 1)
				       (string-equal "Un-archieve" (nth 4 (org-heading-components))))))
      (org-cut-subtree))
    ;; process each tag
    (while tags
	(let* ((tag (nth 0 tags))
	      (remove-flag nil))
	  (save-excursion
	    ;; if exist the level-1 headline of this tag
	    ;;;; narrow region to search the title under this tag headline
	    ;;;;;; if title exist
	    ;;;;;;;; prepare for remove the tag of current entry (set remove-flag)
	    ;;;;;; else
	    ;;;;;;;; insert this title under the tag headline
	    ;; else
	    ;;;; insert this new tag headline
	    ;;;; insert title under this new headline
	    ;; if remove-tag
	    ;;;; remove tag for current entry
	  (if (org-ql-select (buffer-name)
		`(and (level 1) (heading ,tag)))
	      (progn
		(let ((start (re-search-forward (concat "^* " tag "\n") nil t))
		      (end (re-search-forward "^* .*\n" nil t)))
		  (save-restriction
		    (setq start (if start start 1))
		    (setq end (if end end (point-max)))
		    (narrow-to-region start end)
		    (goto-char (point-min))
		    (if (re-search-forward (format "^** %s" (regexp-quote headline)) nil t) ;; no such titles under this tag headline
			;; remove the tag of current entry headline
			(progn
			  (setq remove-flag t)
			  )
		      (progn
			(goto-char (point-max))
			(forward-line -1)
			(insert (format "** %s\n" headline)))
		      )
		    )
		  ))
	    (progn
	      (end-of-buffer)
	      (forward-line)
	      (insert (format "* %s\n" tag))
	      (forward-line 2)
	      (insert (format "** %s\n" headline))
	      )
	    )
	  )
	  (if remove-flag
	      (org-toggle-tag tag 'off))
	)
	(setq tags (cdr tags))
	)
    )))

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

(global-set-key (kbd "<f3>") #'lxs/open-compaction-file)

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


;; randomly pick a theme
(defun random-choice (items)
  (let* ((size (length items))
	 (index (random size)))
    (nth index items)))

(defun lxs/pick-random-theme ()
  (interactive)
  (load-theme (random-choice (custom-available-themes))))

;; (use-package which-function-mode
;;   :hook ((prog-mode . which-function-mode)
;;          (org-mode . which-function-mode))
;;   :init
;;   (setq which-func-unknown "")
;;   (add-hook 'which-function-mode-hook
;;             #'(lambda ()
;;                 (add-to-list 'which-func-functions
;;                              #'(lambda ()
;;                                  (when (eq major-mode 'org-mode)
;;                                    (mapconcat 'identity (org-get-outline-path t)
;;                                               \" > \"))))))
;;   )

;; (add-hook 'prog-mode-hook '(lambda () (setq header-line-format
;;                                        '((which-func-mode (\"\" which-func-format))))))
;; (add-hook 'org-mode-hook '(lambda () (setq header-line-format
;; 					   '((which-func-mode (\"\" which-func-format))))))

(defun lxs-org-is-hugo-file-p (fPath)
  "Predict if the org file has been converted into hugo"
  (with-temp-buffer
    (let ((keyline "#+HUGO_DRAFT: false\n"))
	  (insert-file-contents fPath)
	  (and (search-forward keyline nil t) t)
	  )
    ))

(defun lxs-org-is-hugo-file-p-test ()
  "Predict if the org file has been converted into hugo"
  (interactive)
  (with-temp-buffer
    (let ((keyline "#+HUGO_DRAFT: false\n"))
	  (insert-file-contents "/mnt/c/Users/lixun/Documents/org/org-roam-files/emacs_counsel_find_file_快速输入目标路径.org")
	  (if (and (search-forward keyline nil t) t)
	      (message "yes")
	    (message "no"))
	  )
    ))

(setq lxs-org-hugo-header-info "#+HUGO_BASE_DIR: /mnt/c/Users/lixun/Documents/xssq-blog\n#+HUGO_AUTO_SET_LASTMOD: t\n#+HUGO_TAGS: \n#+HUGO_CATEGORIES: \n#+HUGO_DRAFT: false\n\n")

(defun lxs/org-hugo-export-current-buffer ()
  (interactive)
  (if (lxs-org-is-hugo-file-p (buffer-name))
      (org-hugo--export-file-to-md (buffer-name))
    (progn
      (beginning-of-buffer)
      (insert lxs-org-hugo-header-info)
      (org-hugo--export-file-to-md (buffer-name)
      ))
  ;; (org-hugo--export-file-to-md (buffer-name))
    ))


;; (use-package coin-ticker
;;   ;; 查看我的 shib 涨跌
;;   :load-path "site-lisp/"
;;   :custom
;;   (coin-ticker-count 4723867)
;;   :config
;;   (coin-ticker-mode 1))



;; sql query for org-roam files with Project tag
(with-eval-after-load 'org-roam
  (setq lxs/org-roam-project (org-roam-db-query
                [:select file
                 :from tags
                 :where (like tags (quote "%\"Project\"%"))]))
  )



;; eaf configuration, but it will not work perfectly on wsl1
(use-package eaf
  :disabled t
  :load-path "~/.emacs.default/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding))


;; snippet to change package source
;; (setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;; 			 ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))



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
         (command (format "%s `wslpath -w %s`" (shell-quote-argument app-name) path)))
    (shell-command-to-string command)))

(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
(wsl--open-with reveal-in-explorer "explorer.exe" default-directory)
(wsl--open-with open-in-vscode "Code.exe" buffer-file-name)

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


;;  to test 

(provide 'personal)
