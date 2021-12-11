;; nox, a lightweight lsp client
(use-package nox
  :load-path "site-lisp/nox/"
  :disabled t
  :init
  (defvar nox-python-path "/opt/anaconda/bin/python3")
  ;; (defvar nox-python-path "/opt/anaconda/bin/python")
  ;; 对于 nox+mspyls, 需要指定 python 解释器的路径， vitural envs 似乎不能够起到作用
  ;; (defvar lsp-python "/home/lixunsong/anaconda3/envs/py-emacs/bin/python3.7")
  ;; (defvar lsp-search-paths [])

  ;; :custom
  ;;
  :config
  ;; cpp
   (add-to-list 'nox-server-programs '(c++-mode . ("clangd")))
   (add-to-list 'nox-server-programs '(c-mode . ("clangd")))
   ;; (add-to-list 'nox-server-programs '((python-mode) . "pyright"))
   (setq nox-python-server "pyright")
   (define-key nox-mode-map (kbd "C-.")  'nox-show-doc)
   )

;; (use-package nox
  ;; :load-path "site-lisp/nox/")
;; (setq nox-server-programs (remove (nth 0 nox-server-programs) nox-server-programs))

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
  :disabled t
  :init
  (defun lxs/generate-ctags-for-python-project ()
    (interactive)
    (shell-command "ctags --languages=python --kinds-all='*' --fields='*' --extras='*' -R"))
  ;; custom citre backends
  (defun company-citre (-command &optional -arg &rest _ignored)
  "Completion backend of for citre.  Execute COMMAND with ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case -command
    (interactive (company-begin-backend 'company-citre))
    (prefix (and (bound-and-true-p citre-mode)
                 (or (citre-get-symbol) 'stop)))
    (meta (citre-get-property 'signature -arg))
    (annotation (citre-capf--get-annotation -arg))
    (candidates (all-completions -arg (citre-capf--get-collection -arg)))
    (ignore-case (not citre-completion-case-sensitive))))
  )

;; (setq python-shell-interpreter "/home/lixunsong/anaconda3/bin/python3.8")
;; python
(use-package pyvenv
  :ensure t
  :disabled t
  :hook (python-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" "/opt/anaconda/envs")
  :config
  (pyvenv-workon "py39")
  )

(use-package helm-dash
  :ensure t
  :disabled t
  :bind
  (:map prog-mode-map ("M-[" . helm-dash-at-point))
  :config
  (setq helm-dash-common-docsets '("Python 3" "PyTorch"))
  (setq helm-dash-browser-func 'eww)
  )

;; leetcode
(use-package leetcode
  :config
  (setq leetcode-prefer-language "python3"
	leetcode-save-solutions t
	leetcode-directory (concat lxs-home-dir "leetcode")))

;;
(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
