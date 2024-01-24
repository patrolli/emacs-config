(defconst pelican-date-time-format "%Y%m%dT%H%M%S")
(defconst pelican-date-format "%Y%m%d")

(defvar pelican-keywords-regexp "^[Tt]ags:[[:space:]]*\\(.+?\\)$"
  "Regular expression for matching Pelican-style tags in front matter.")

(defvar pelican-slug-regexp "^[Ss]lug:[[:space:]]*\\(.+?\\)$"
   "Regular expression for matching Pelican slug in front matter.")

(defvar pelican-current-session-tags nil)

(defvar pelican-content-path
	(if (eq system-type 'windows-nt)
	"C:/Users/xunsong.li/SynologyDrive/project/website/content"
	"~/SynologyDrive/project/website/content"))

(defvar pelican-yaml-front-matter 
  "---
title:      %s
date:       %s
tags:       
---\n\n"
  "YAML (Markdown) front matter.
It is passed to `format' with arguments TITLE, DATE, KEYWORDS,
ID.  Advanced users are advised to consult Info node `(denote)
Change the front matter format'.")

(defun pelican-directory-files ()
  "返回 `pelican-content-path' 目录下所有的 md 文件" 
  (directory-files-recursively pelican-content-path ".md$"))

(defun pelican-open-or-create (target)
  "Open or create a Markdown (.md) file in the specified DIRECTORY.
List existing .md files and allow narrowing. If the entered
filename doesn't exist, create a new file."
  (interactive (list (pelican-file-prompt)))
  (let* ((target-file (file-name-concat pelican-content-path target)))
    (if (file-exists-p target-file)
	(find-file target-file)
	(pelican-create-file target))))

(defun pelican-create-file (title)
  (find-file (file-name-concat (file-name-concat pelican-content-path (format "%s--%s.md" (format-time-string pelican-date-format) (pelican-sluggify title)))))
  (insert (pelican--format-front-matter title (format-time-string pelican-date-time-format))))

(defun pelican--format-front-matter (title date)
  (format pelican-yaml-front-matter title date))

(defun pelican-file-prompt (&optional initial-text)
  "Prompt for file with identifier in variable `denote-directory'.
With optional INITIAL-TEXT, use it to prepopulate the minibuffer."
  (let ((md-files (directory-files pelican-content-path t "\\.md$"))
	)
	(completing-read "Open or create .md file: " (mapcar #'file-name-nondirectory md-files) nil nil)))

(defun pelican--inferred-keywords ()
  (setq pelican-current-session-tags '())
  (let ((files (directory-files-recursively pelican-content-path "\\.md$")))
    (dolist (file files)
      (when (file-regular-p file)
	(with-current-buffer (find-file-noselect file)
	  (setq pelican-current-session-tags (append pelican-current-session-tags (pelican--keywords-in-cur-buffer))))))
    (setq pelican-current-session-tags (delete-dups pelican-current-session-tags))
    pelican-current-session-tags))

(defun pelican-keywords ()
  ;; 得到 contents/ 下的文件中全部的 tag
  (delete-dups (pelican--inferred-keywords)))

(defun pelican--keywords-in-cur-buffer ()
  "Extract tags from the front matter of the current buffer."
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "^[T\\|t]ags[:]?" nil t)
    (beginning-of-line)
    (when (re-search-forward pelican-keywords-regexp (+ 1 (line-end-position)) t)
      (let* ((tags-str (match-string-no-properties 1))
             (tags (if (string-blank-p tags-str) nil (split-string tags-str "[[:space:],]+"))))
        (message "Tags: %s" tags)
	tags))))

(defun pelican--slug-in-cur-buffer ()
    (save-excursion
	(beginning-of-buffer)
	(re-search-forward "^[S\\|s]lug[:]?" nil t)
	(beginning-of-line)
	(when (re-search-forward pelican-slug-regexp (+ 1 (line-end-position)) t)
	    (let* ((slug-str (match-string-no-properties 1)))
	    slug-str))))

(defun pelican-insert-tag ()
  (interactive)
  (if (eq pelican-current-session-tags nil)
      (pelican-keywords)
    (let ((new-tag (completing-read "Select tags: " (pelican-keywords)))
	  (cur-tags (pelican--keywords-in-cur-buffer)))
      ;; TODO: 将选择的 tag insert 到 buffer 中
      ;; 首先获取当前 buffer 的 tags, 如果选择的 tag 在其中，就不操作，否则加入 tags，将 tags 一行进行重写
      (unless (member new-tag cur-tags)
	(pelican--rewrite-keywords (buffer-file-name) (push new-tag cur-tags))))))

(defun pelican--rewrite-keywords (file kwds)
  ;; TODO: slugify input keywords, wrap with \"\"
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^tags\\s-*:" nil t 1)
          (goto-char (line-beginning-position))
	  (kill-line)
          (insert "tags:        ")
	  (cond
	    ((<= (length kwds) 1)
	    (when kwds
		(insert (car kwds))))
	    (t
	    (dolist (kwd (butlast kwds))
		(insert (format "%s, " kwd)))
	    (insert (car (last kwds)))))
	  (goto-char (line-end-position))))))


;; slugify titles, borrowed from denote
(defconst pelican-excluded-punctuation-regexp "[][{}!@#$%^&*()=+'\"?,.\|;:~`‘’“”/]*"
  "Punctionation that is removed from file names.
We consider those characters illegal for our purposes.")

(defun pelican--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string
   pelican-excluded-punctuation-regexp "" str))

(defun pelican--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

(defun pelican-sluggify (str)
  "Make STR an appropriate slug for file names and related."
  (downcase (pelican--slug-hyphenate (pelican--slug-no-punct str))))

(defun pelican-open-in-browser ()
  (interactive)
  (let ((slug (pelican--slug-in-cur-buffer)))
    (if slug
	(browse-url (format  "http://127.0.0.1:5500/%s.html" slug)))
    ))

;; bind-keys
(global-set-key (kbd "C-c 1") #'pelican-open-or-create)


;; 添加 modified time

;; TODO: 文件管理可以简单通过 dired 实现，而不需要额外写一些函数
;; (dired pelican-content-path)

;; DONE: 通过命令插入或删除 tag，支持从已有的 tag 中补全

;; TODO: 图片的添加和管理，需要 markdown 那边的配合
;; 把图片放到 images/ 目录下，按放入的时间排序，然后提供一个命令，显示图片？
;; DONE: 文件名创建时加上日期，便于搜索排序

(format-time-string "%Y%m%dT%H%M" (file-attribute-modification-time (file-attributes (buffer-file-name))))

(provide 'init-pelican)
