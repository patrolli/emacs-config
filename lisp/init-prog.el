;; nox, a lightweight 
(use-package nox
  :load-path "~/.emacs.d/nox/"
  :init
  ;; (defvar lsp-python "/usr/bin/python3.8")
  ;; 对于 nox+mspyls, 需要指定 python 解释器的路径， vitural envs 似乎不能够起到作用
  (defvar lsp-python "/home/lixunsong/anaconda3/envs/py-emacs/bin/python3.7")
  (defvar lsp-search-paths [])
  :config
  ;; add hooks TODO: why :hook failed?
   (dolist (hook (list
                 'js-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'haskell-mode-hook
                 ))
    (add-hook hook '(lambda () (nox-ensure))))
  ;; cpp 
  (add-to-list 'nox-server-programs '(cpp-mode . ("clangd")))
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
  (add-to-list 'nox-server-programs
             `(python-mode nox-mspyls 
                           "~/.emacs.d/nox/mspyls/Microsoft.Python.LanguageServer"))
  ;; pyls configuration is simple~
  ;; (add-to-list 'nox-server-programs '(python-mode . ("python-language-server" "pyls")))
  )


(defun xref--pop-to-location@around (func item &optional action)
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker)))
    (with-current-buffer buf
      (read-only-mode)))
  (funcall func item action))
(advice-add 'xref--pop-to-location :around #'xref--pop-to-location@around)


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

 (advice-add 'xref--pop-to-location :around #'xref--pop-to-location@around)

;; python
(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" "/home/lixunsong/anaconda3/envs"))

(use-package lsp-mode
  :ensure t)

(provide 'init-prog)
