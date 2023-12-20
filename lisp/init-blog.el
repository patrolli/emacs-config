
(defvar myblog-content-path
	(if (eq system-type 'windows-nt)
	"C:/Users/xunsong.li/Documents/project/website/content"
	"~/Documents/project/website/content"))
(defun myblog-export-current-buffer ()
  "Copy the current Markdown file to DESTINATION-FOLDER."
  (interactive)
  (let ((buffer-file-name (buffer-file-name)))
    (if (and buffer-file-name (string-suffix-p ".md" buffer-file-name))
        (let* ((file-name (file-name-nondirectory buffer-file-name))
               (destination-file (expand-file-name file-name myblog-content-path)))
          (copy-file buffer-file-name destination-file t)
          (message "Markdown file copied successfully to %s" destination-file))
      (message "Not a Markdown file or buffer is not visiting a file"))))

(defun myblog-is-published-file ()
  "Extract the 'status' parameter from the Markdown front matter."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^status:[[:space:]]*\\(.*\\)" nil t)
      (match-string 1))))
(defun myblog-auto-export-current-buffer ()
	(let* ((buffer-file-name (buffer-file-name))
         (status (myblog-is-published-file)))
    (if (and buffer-file-name (string-suffix-p ".md" buffer-file-name) (string-equal status "published"))
        (let* ((file-name (file-name-nondirectory buffer-file-name))
               (destination-file (expand-file-name file-name myblog-content-path)))
          (copy-file buffer-file-name destination-file t)
          (message "Auto export to blog..." destination-file))
      (message ""))
	  ))
	  
(defun myblog-export-file-after-save ()
  "Automatically copy the Markdown file to the predefined target folder after saving."
  (add-hook 'after-save-hook #'myblog-auto-export-current-buffer nil t))
;; Set up the hook
(add-hook 'markdown-mode-hook #'myblog-export-file-after-save)

(provide 'init-blog)
