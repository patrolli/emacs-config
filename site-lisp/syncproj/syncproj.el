(require 'init-const)

(defvar remote-base-path "~/DL_Workspace"
  "parent folder to sync local project")

  ;; (defvar rsync-cmd-template "rsync -auvz %s --delete --exclude-from=./rsync_exclude.txt --timeout=5  %s %s:%s/%s;")
  ;; proxychains rsync -auvz  --timeout=5  ~/.emacs.d/site-lisp/rsync-project/ 2:~/DL_Workspace/rsync-project-2;
(defvar rsync-cmd-template "rsync -auvz %s --timeout=5  %s %s:%s/%s;"
  "1: -n 2: local project path 3: remote name 4: remote base path 5: poject name")

;; "\033[31;1;4m$(date)\033[0m\n"
(defvar rsync-add-on-cmd "echo -e \"\\033[31;1;4m$(date)\\033[0m\n\"")

(defvar syncproj-when-changed-cmd-templeate (if sys/linuxp "when-changed -rv1s %s -c \"%s\""
					      "wsl ~/.local/bin/when-changed -rv1s %s -c %s")
    "cmd template for when-changed")

(defvar wsl-home-path "/mnt/c/users/xunsong.li"
  "access windows home path in wsl")

(defun convert-windows-home-to-wsl (fpath)
  "convert C:\\Users\\lixun\\path\\to\\file to /mnt/c/Users/lixun/path/to/file"
  (let* ((str1 (replace-regexp-in-string "[Cc]:\\\\+[Uu]sers\\\\+xunsong\\.li" wsl-home-path fpath))
	 (str2 (replace-regexp-in-string "[Cc]:/+[Uu]sers/+xunsong\\.li" wsl-home-path fpath))
	 (str3 (subst-char-in-string ?\\ ?/ str2)))
    str3))

(convert-windows-home-to-wsl "c:/Users/xunsong.li/Documents/python_work/mmdetection/")
;c:/Users/xunsong.li/Documents/python_work/mmdetection/

(defun syncproj-initialize-project ()
  (interactive)
  (let* ((p-path (project-root (project-current t)))
         (remote (completing-read "Remote repo: "
                                  (split-string
                                   (shell-command-to-string
                                    (if sys/linuxp "cat ~/.ssh/config | grep \"^Host \" | awk '{print $2}'"
				      "powershell cat ~/.ssh/config | wsl grep \"^Host \" | wsl awk '{print $2}'")
				    ))))
           (p-name (concat (file-name-nondirectory (directory-file-name p-path))
                           "-" remote)))
      (cond ((syncproj-is-sync-project p-name) (message "%s is rsyncing!" p-name))
            ((not (syncproj-has-exclude-file? p-path)) (progn
                                                   (message "exclude.txt not found! Edit it and re-run this command")
                                                   (syncproj-make-exclude-file p-path)))
            (t (let ((out-buf (get-buffer-create (format "*my-rsync-%s*" p-name))))
                 (syncproj-run p-path p-name remote out-buf))))))


(defun syncproj-dry-run (p-path p-name remote out-buf)
  "run a test to check which files are rsync"
  (let* ((rsync-cmd (format rsync-cmd-template "-n" p-path remote remote-base-path p-name))
         (cmd (format syncproj-when-changed-cmd-templeate p-path rsync-cmd)))
    (async-shell-command cmd out-buf nil)))

(defun syncproj-run (p-path p-name remote out-buf)
  "如果项目中有 .syncproj 文件，那么读取这个文件的 rsync 命令
  如果没有的话，就使用默认的 rsync 命令模板"
  (let* ((sync-f (file-name-concat p-path ".syncproj"))
         rsync-cmd wc-cmd)
    (if (file-exists-p sync-f)
        (setq rsync-cmd (syncproj-parse-file sync-f remote))
      (setq rsync-cmd (format rsync-cmd-template "" p-path remote remote-base-path p-name)))
    (if sys/win32p
	(setq p-path (convert-windows-home-to-wsl (file-truename p-path)))
      )
    (setq wc-cmd (format syncproj-when-changed-cmd-templeate p-path rsync-cmd))
    (async-shell-command wc-cmd out-buf nil)
    (message p-path)))

(defun syncproj-parse-file (f-path remote)
  "replace '<remote>' in .syncproj with selected remote name"
    (let* ((cmd-tmp (f-read-text f-path))
           (regex "\\\s\\(<remote>\\):"))
      (with-temp-buffer
        (insert cmd-tmp)
        (goto-char (point-min))
        (while (re-search-forward regex nil t)
          (replace-match remote nil nil nil 1))
         (buffer-string))))

(defun syncproj-list-sync-projects ()
  "得到当前所有 my-rsync-* buffer"
  (let* (
         (bufs (remove-if-not #'(lambda (x) (string-prefix-p "*my-rsync-" (buffer-name x))) (buffer-list))))
    (or nil bufs)))

(defun syncproj-has-exclude-file? (p-path)
  (file-exists-p (concat p-path "/rsync_exclude.txt")))

  (defun syncproj-make-exclude-file (p-path)
    "if exclude.txt does not exist,
  create and jump to it"
    (let* ((fpath (concat p-path "/rsync_exclude.txt")))
      (if (file-exists-p fpath)
          (find-file fpath)
        (with-temp-buffer (write-file fpath nil)))))

(defun syncproj-is-sync-project (proj-name)
  "check if project (name) being rsync now
  if is, return the rsync buffer, else nil
  TODO: predicate should be improved!"
  (if-let (proj-bufs (syncproj-list-sync-projects))
      (progn
        (car (cl-remove-if-not #'(lambda (x) (string-match-p proj-name (buffer-name x))) proj-bufs)))))

(defun syncproj-shutdown--project (proj-name)
  "给定 project 的名称，然后找到其对应的 sync buffer, 然后杀掉这个进程"
  (when-let* ((buf (syncproj-is-sync-project proj-name)))
    (when-let (process (get-buffer-process buf))
      (kill-process process)
      (sleep-for 0.1)) ;; wait until the process has been killed
    (kill-buffer buf)
    (message "kill rsync project %s" proj-name)))

(defun syncproj-shutdown-project ()
  (interactive)
  (if-let ((proj-bufs (syncproj-list-sync-projects))
           (choices (mapcar #'(lambda (x) (apply 'format "%s<%s>" (syncproj-parse-proc-buf-name (buffer-name x)))) proj-bufs))
           (chosen (completing-read "choose to shutdown: " choices))
           (idx (-elem-index chosen choices))
           (chosen-buf (nth idx proj-bufs)))
      (syncproj-shutdown--project (car (syncproj-parse-proc-buf-name (buffer-name chosen-buf))))))


(defun syncproj-parse-proc-buf-name (bufname)
  "将 *my-rsync-rsync-project-2* 这样的 buffer name
  解析出 project name (rsync-project) 和 remote name (2)"
  (save-match-data
    (if (string-match "\\*my-rsync-\\(.+\\)-\\(.+\\)\\*" bufname)
        (progn (setq proj-name (match-string 1 bufname))
               (setq remote (match-string 2 bufname))
               (list proj-name remote))
      (list nil nil))))

  ;; tests
(syncproj-shutdown--project "rsync-project")
  ;; (get-buffer-create "*my-rsync-test*")
  ;; (kill-process (get-buffer-process (syncproj-is-sync-project "rsync-project")))
  ;; (get-buffer-process "*my-rsync-rsync-project*")
  ;; (kill-buffer "*my-rsync-rsync-project*")
