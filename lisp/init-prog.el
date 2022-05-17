(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (setq read-process-output-max (* 1024 1024))
  (setq eldoc-echo-area-use-multiline-p nil))

;; (setq python-shell-interpreter "/home/lixunsong/anaconda3/bin/python3.8")
;; python
(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  ;; :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (pyvenv-workon "py39")
  )

;; leetcode
(use-package leetcode
  ;; :hook
  ;; (after-init . leetcode)
  :load-path ("site-lisp/leetcode.el")
  :config
  (setq leetcode-prefer-language "python3"
	leetcode-save-solutions t
	leetcode-directory (concat lxs-home-dir "leetcode"))
  ;; 在 leetcode mode 中，给当前做的题目快速添加一个 roam note
  (defun leetcode-take-notes ()
    (interactive)
    (when-let* ((buf (get-buffer "*leetcode-description*")))
      (with-current-buffer "*leetcode-description*"
	(goto-char (point-min))
	(let* ((headline (string-trim (buffer-substring-no-properties (point-min) (point-at-eol))))
	       (title (string-trim (car (last(split-string headline "\\."))))) ;; 141. Linked List Cycle
	       (url (concat "https://leetcode-cn.com" "/problems/" (leetcode--slugify-title title))))
	  (org-roam-capture-
	   :keys "l"  ;; leetcode 对应的 roam capture 模板
	   :node (org-roam-node-create
		  :title headline)
	   :info (list :ref url)
	   :templates org-roam-capture-ref-templates)
	  )))
  ))

(add-hook 'prog-mode-hook 'show-paren-mode)

(provide 'init-prog)
