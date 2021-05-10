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
  (let ((cmd (format "jupyter-lab --no-browser --port 8888 --notebook-dir %s" default-directory)))
    (if (get-process "jupyter")
	(if (y-or-n-p "Existing jupyter lab was found, replace(y) it?")
	    (progn
	      (start-process-shell-command "jupyter" nil cmd)
	      (browse-url "http://localhost:8888"))
	  (browse-url "http://localhost:8888"))
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

(provide 'init-funcs)
