;; nox, a lightweight lsp client
(use-package nox
  :load-path "site-lisp/nox/"
  ;; :disabled t
  :init
  ;; (defvar lsp-python "/usr/bin/python3.8")
  ;; 对于 nox+mspyls, 需要指定 python 解释器的路径， vitural envs 似乎不能够起到作用
  (defvar lsp-python "/home/lixunsong/anaconda3/envs/py-emacs/bin/python3.7")
  (defvar lsp-search-paths [])
  ;; :hook
  ;; (after-init . nox)
  ;; 不能在这里设置按键绑定，这里设置会导致 nox 延迟加载，而实际上 nox 之后被不会加载
  ;; 因为 hook 里面没有添加启动 nox 的函数，不知道添加过后还会不会出现这个情况
  ;; :custom
  ;; (nox-python-server "pyright")
  :config

  ;; cpp 
   (add-to-list 'nox-server-programs '(c++-mode . ("clangd")))
   (add-to-list 'nox-server-programs '(c-mode . ("clangd")))
  ;; python + mspyls
  ;; for mspyls, we need 3 steps to prepare:
  ;; 1. Execute command 'nox-print-mspyls-download-url' get download url of mspyls.
  ;; 2. Then extract to the directory ~/.emacs.d/nox/mspyls/
  ;; 3. Permission: ```sudo chmod -R +x ~/.emacs.d/nox/mspyls/
  ;; The following snippet is needed necessary
  (defclass nox-mspyls (nox-lsp-server) ()
    :documentation "MS Python Language Server.")
  (setq-default nox-workspace-configuration
              '((:python :autoComplete (:extraPaths nil)
                         :analysis (:autoSearchPaths :json-false :usePYTHONPATH :json-false))))
  (cl-defmethod nox-initialization-options ((_server nox-mspyls))
  `(:interpreter
    (:properties
     (:InterpreterPath ,lsp-python))
     :searchPaths ,lsp-search-paths))
  (add-to-list 'nox-server-programs '((python-mode) "pyright-langserver" "--stdio"))
  (define-key nox-mode-map (kbd "C-.")  'nox-show-doc)
  )



(defun xref--pop-to-location@around (func item &optional action)
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker))
         (file (xref-location-group (xref-item-location item)))
         (directory (projectile-project-root)))
    (if (not (string-match directory file))
        (with-current-buffer buf
          (read-only-mode)))
    (funcall func item action)
    ))

(use-package citre
  :diminish
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :init
  (defun lxs/generate-ctags-for-python-project ()
    (interactive)
    (shell-command "ctags --languages=python --kinds-all='*' --fields='*' --extras='*' -R"))
  )

(setq python-shell-interpreter "/home/lixunsong/anaconda3/bin/python3.8")
;; python
(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" "/home/lixunsong/anaconda3/envs")
  :config
  (pyvenv-workon "py-emacs"))

(use-package helm-dash
  :ensure t
  :disabled t
  :bind
  (:map prog-mode-map ("M-[" . helm-dash-at-point))
  :config
  (setq helm-dash-common-docsets '("Python 3" "PyTorch"))
  (setq helm-dash-browser-func 'eww)
  )

;; (use-package smartparens-config
;;   :hook (prog-mode . smartparens-mode))

;; (add-hook 'prog-mode-hook (lambda () (setq which-function-mode t)))
;; 
(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
