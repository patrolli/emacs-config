
 ;; cl - Common Lisp Extension
(require 'cl-lib)
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Load `custom-file'
(load custom-file)

;; Load custom-post-file
(defun load-custom-post-file ()
"Load custom-post file."
(cond ((file-exists-p lxs-custom-post-org-file)
       (and (fboundp 'org-babel-load-file)
            (org-babel-load-file lxs-custom-post-org-file)))
      ((file-exists-p lxs-custom-post-file)
       (load lxs-custom-post-file))))
(add-hook 'after-init-hook #'load-custom-post-file)

(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; (require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)



(cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
  (use-package benchmark-init
    :config
    (require 'benchmark-init-modes)                                     ; explicitly required
    (add-hook 'after-init-hook #'benchmark-init/deactivate)))


;; (use-package benchmark-init
  ;; :ensure t
  ;; :config
  ;; To disable collection of benchmark data after init is done.
  ;; (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'init-packages)


