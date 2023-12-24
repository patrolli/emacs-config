(defconst pelican-date-format "%Y%m%dT%H%M%S")

(defvar pelican-keywords-regexp "^[T\\|t]ags[:]? [=]?+.*\\[\\(.+?\\)\\]$")

(defvar pelican-current-session-tags nil)

(defvar pelican-content-path
	(if (eq system-type 'windows-nt)
	"C:/Users/xunsong.li/Documents/project/website/content"
	"~/Documents/project/website/content"))

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
  (directory-files-recursively pelican-content-path ".md$"))

(defun pelican-open-or-create (target)
  "Open or create a Markdown (.md) file in the specified DIRECTORY.
List existing .md files and allow narrowing. If the entered
filename doesn't exist, create a new file."
  (interactive (list (pelican-file-prompt)))
  (let* ((target-file (file-name-concat pelican-content-path target)))
    (if (file-exists-p target-file)
	(find-file target-file)
	(pelican-create-file target))
    )
  )

(defun pelican-create-file (title)
  (find-file (file-name-concat (file-name-concat pelican-content-path (format "%s.md" title))))
  (insert (pelican--format-front-matter title (format-time-string pelican-date-format))))

(defun pelican--format-front-matter (title date)
  (format pelican-yaml-front-matter title date)
  )

(defun pelican-file-prompt (&optional initial-text)
  "Prompt for file with identifier in variable `denote-directory'.
With optional INITIAL-TEXT, use it to prepopulate the minibuffer."
  (let ((md-files (directory-files pelican-content-path t "\\.md$"))
	)
	(completing-read "Open or create .md file: " (mapcar #'file-name-nondirectory md-files) nil nil)
  ))

(defun pelican--inferred-keywords ()
  (let ((files (directory-files-recursively pelican-content-path ".md$")))
    (let ((source (with-temp-buffer
		    (while files
		      (insert-file-contents (car files))
		      (pop files))
		    (buffer-string))))
      (save-match-data
	(let ((pos 0)
	      matches)
	  (while (string-match "^[T\\|t]ags[:]? [=]?+.*\\[\\(.+?\\)\\]$" source pos)
	    (push (match-string 1 source) matches)
	    (setq pos (match-end 0)))
	  (setq pelican-current-session-tags matches)))))
  )

(defun pelican-keywords ()
  ;; 得到 contents/ 下的文件中全部的 tag
  (delete-dups (pelican--inferred-keywords)))

(defun pelican--keywords-in-cur-buffer ()
  (with-current-buffer (buffer-name)
      (save-excursion
      (goto-char (point-min))
    (save-match-data
      (let ((source (buffer-string))
	    (pos 0)
	    matches)
	(while (string-match pelican-keywords-regexp source pos)
	  (push (match-string-no-properties 1 source) matches)
	  (setq pos (match-end 0))
	  )
	matches))))
  )

(defun pelican-insert-tag ()
  (interactive)
  (if (eq pelican-current-session-tags nil)
      (pelican-keywords)
    (let ((new-tag (completing-read "Select tags: " pelican-current-session-tags))
	  (cur-tags (pelican--keywords-in-cur-buffer)))
      ;; TODO: 将选择的 tag insert 到 buffer 中
      ;; 首先获取当前 buffer 的 tags, 如果选择的 tag 在其中，就不操作，否则加入 tags，将 tags 一行进行重写
      (unless (member new-tag cur-tags)
	(pelican--rewrite-keywords (buffer-file-name) (push new-tag cur-tags))
	)
      )
    )
  )

(defun pelican--rewrite-keywords (file kwds)
  ;; TODO: slugify input keywords, wrap with \"\"
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^tags\\s-*:" nil t 1)
          (goto-char (line-beginning-position))
	  (kill-line)
          (insert "tags:        [")
	  (when kwds
	    (insert (format "%s" (car kwds)))
	    (dolist (kwd (cdr kwds))
	      (insert (format " %s" kwd)))
	    )
	  (insert "]")
	  (goto-char (line-end-position)))
      )
    )
  )



;; bind-keys
(global-set-key (kbd "C-c 1") #'pelican-open-or-create)


;; 添加 modified time

;; TODO: 文件管理可以简单通过 dired 实现，而不需要额外写一些函数
;; (dired pelican-content-path)

;; DOING: 通过命令插入或删除 tag，支持从已有的 tag 中补全

;; TODO: 图片的添加和管理，需要 markdown 那边的配合
;; 把图片放到 images/ 目录下，按放入的时间排序，然后提供一个命令，显示图片？
