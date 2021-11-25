(require 'init-custom)
;; (require 'init-basic)


(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and
   (display-graphic-p)
   (require 'all-the-icons nil t)))

(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" lxs-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,lxs-proxy)
          ("https" . ,lxs-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)                ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1086 5))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

;; 先 list-process 看 jupyter 是否已经运行了
;; 如果运行了，显示提示符，询问是否删除之前的 jupyter 进程，或者沿用
;; password 813729
(defun lxs/open-jupyter-in-chrome ()
  (interactive)
  (let ((cmd (format "jupyter-lab --no-browser --port 8888 --notebook-dir %s" "~/")))
    (if (get-process "jupyter")
	(if (y-or-n-p "Existing jupyter lab was found, reuse(y) it?")
	    (browse-url "http://localhost:8888")
	    (progn
	      (start-process-shell-command "jupyter" nil cmd)
	      (browse-url "http://localhost:8888"))
	  )
      (progn
	(start-process-shell-command "jupyter" nil cmd)
	(browse-url "http://localhost:8888")))
	)
  )

(defun lxs/kill-jupyter-in-browser ()
  (interactive)
  (let ((process (get-process "jupyter")))
    (if process
	(progn
	  (kill-process process)
	  (message "jupyter is killed!"))
      (message "no jupyter is running!"))))

(defun my/swap-org-ref-cite-title ()
					;TODO: 处理标题行带 tag 的情况
  ;TODO: 执行完后移动光标到下一行
  (interactive)
  (save-restriction
    (beginning-of-line)
    (let* ((lbegin (line-beginning-position 1))
	   (lend (line-beginning-position 2))
	   (ip (re-search-forward "\\(*.? \\|- \\)" nil t nil))
	   (cp (re-search-forward "cite:.*? " nil t nil))
	   )   
      (narrow-to-region lbegin lend)
      (beginning-of-line)
      (kill-region cp (- lend 1))
      (goto-char ip)
      (yank)
      (insert " ")
      )
    )
  )

(defun centaur-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse url with webkit and switch or pop to the buffer.
POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (when (and (featurep 'xwidget-internal)
             (fboundp 'xwidget-buffer)
             (fboundp 'xwidget-webkit-current-session))
    (xwidget-webkit-browse-url url new-session)
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (if pop-buffer
            (pop-to-buffer buf)
          (switch-to-buffer buf))))))

(centaur-webkit-browse-url "www.baidu.com")

(provide 'init-funcs)
