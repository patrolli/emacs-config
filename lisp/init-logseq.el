(defvar logseq-page-dir "~/Documents/logseq/pages")
(defvar logseq-journal-dir "~/Documents/logseq/journals")
(defvar logseq-bak-dir "~/Documents/logseq/logseq/bak/")

(defun xs/logseq-open-page ()
  "创建或打开一个 logseq 的 org 文件"
  (interactive)
  (ido-find-file-in-dir logseq-page-dir))

(defun xs/logseq-open-journal ()
  "打开一个 logseq 的日志文件"
  (interactive)
  (ido-find-file-in-dir logseq-journal-dir))

(defun xs/logseq-refactor-file ()
  (interactive)
  (let* ((initial-file (buffer-file-name))
	 (initial-slug (file-name-base initial-file))
	 (new-slug (read-string "Refactor: " initial-slug))
	 (new-file (concat
		    (expand-file-name new-slug logseq-page-dir)
		    ".org")))
    (rename-file initial-file new-file)
    (kill-current-buffer)
    (find-file new-file)))

(defun xs/logesq-clear-bak-pages ()
  "清除掉 logseq/bak 的备份文件"
  (interactive)
  (delete-directory logseq-bak-dir t t)
  )

(defun xs/logseq-image-from-clipboard ()
  (interactive)
  (let* ((org-download-screenshot-method "convert clipboard: %s")
	 (save-image-path (format "../assets/image_%s_%s.png" (file-name-base (buffer-name)) (format-time-string "%Y-%m-%d_%H-%M-%S")))
	 (logseq-save-image-path (replace-regexp-in-string "\s+" "_" (downcase save-image-path))))
    (shell-command-to-string
     (format org-download-screenshot-method
	     logseq-save-image-path))
    (insert (format "[[%s]]" logseq-save-image-path))
    )
  )

(defun logseq-goto-today ()
  (interactive)
  (let ((today-fn (concat (format-time-string "%Y_%m_%d") ".org"))
	)
    (find-file (file-name-concat logseq-journal-dir today-fn)))
)

(defun logseq-find-today ()
  (let ((today-fn (concat (format-time-string "%Y_%m_%d") ".org"))
	)
    (set-buffer (find-file-noselect (file-name-concat logseq-journal-dir today-fn))
		)
    (end-of-buffer)))
