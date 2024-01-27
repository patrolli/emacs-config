(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package company
  ;:disabled t				
  :init
  (setq   company-idle-delay 0.1
	  company-minimum-prefix-length 2
	  company-backends '((company-capf :separate company-dabbrev :with company-yasnippet))
                           ;; (company-dabbrev-code company-keywords company-files)
                           ;; company-dabbrev)
	  company-global-modes '(not message-mode help-mode
				     eshell-mode shell-mode)
	  company-show-numbers t
  )
  :bind
  (:map company-active-map
	("M-n" . nil)
	("M-p" . nil)
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("<backtab>" . my-company-yasnippet)
	:map company-mode-map
         ("<backtab>" . company-yasnippet))
  :hook (after-init . global-company-mode)
  :config
  ;; company-dabbrev 不要补全中文
  (setq company-dabbrev-char-regexp "[-_A-Za-z0-9]")
  ;; 只补全当前 buffer 的 dabbrev
  (setq company-dabbrev-other-buffers nil)
  ;; 保持大小写敏感
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (add-to-list 'company-frontends #'company-echo-metadata-frontend)

  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))
  ;; `yasnippet' integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fun command arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))
  )

(provide 'init-company)
