(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package company
  :init (global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  :bind
  (:map company-active-map
	("M-n" . nil)
	("M-p" . nil)
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("<backtab>" . my-company-yasnippet)
	:map company-mode-map
         ("<backtab>" . company-yasnippet))
  ;; :hook (after-init . global-company-mode)
  :config
  (setq company-show-numbers t)
  (setq
   company-global-modes '(not erc-mode message-mode help-mode
                              gud-mode eshell-mode shell-mode org-mode)
   company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)
   )
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

(use-package company-tabnine
  :ensure t
  :custom
  ((company-tabnine-max-num-results 3))
  ;; :bind
  ;; ("M-q" . company-other-backend)
  :hook
  ((nox-managed-mode . (lambda ()
                      (setq company-tabnine-max-num-results 3)
                      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                      (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))))
  ;; (after-init . (lambda () (call-interactively 'company-tabnine))) ;; activate tabnine server when emacs start
  (kill-emacs . company-tabnine-kill-process)
  )
  :init
  ;; Enable TabNine on default
  ;; (add-to-list 'company-backends #'company-tabnine)
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))
  (defun lxs/toggle-tabnine ()
    "temporally close or open tabnine, for saving cpu cost."
    (interactive)
    (if company-tabnine--disabled
	(progn (setq company-tabnine--disabled nil)
	       (message "tabnine enabled"))
      (setq company-tabnine--disabled t)
      (message "tabnine disabled!")
      )
    )
  )

  ;; Icons and quickhelp
(when emacs/>=26p
  (use-package company-box
      :ensure t
      :diminish
      :defines company-box-icons-all-the-icons
      :hook (company-mode . company-box-mode)
      :init (setq company-box-enable-icon t
                  company-box-backends-colors nil
                  company-box-doc-delay 0.3
		  company-box-doc-enable nil)
      :config
      (with-no-warnings
        ;; Prettify icons
        (defun my-company-box-icons--elisp (candidate)
          (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
            (let ((sym (intern candidate)))
              (cond ((fboundp sym) 'Function)
                    ((featurep sym) 'Module)
                    ((facep sym) 'Color)
                    ((boundp sym) 'Variable)
                    ((symbolp sym) 'Text)
                    (t . nil)))))
        (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

      ;; 让 company-box 支持 nox      
      (defun my/company-box-icons--nox (candidate)
	(-when-let* ((nox-item (get-text-property 0 'nox--lsp-item candidate))
		     (kind-num (plist-get nox-item :kind)))
	  (alist-get kind-num company-box-icons--lsp-alist)))
      
      (add-to-list 'company-box-icons-functions 'my/company-box-icons--nox)

      (when (icons-displayable-p)
        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (declare-function all-the-icons-octicon 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
                (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
                (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
                (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
                (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
                (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
                (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
              company-box-icons-alist 'company-box-icons-all-the-icons))))


(provide 'init-company)
