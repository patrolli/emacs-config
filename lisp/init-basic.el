(require 'init-const)

(if sys/wslp (setq
      cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
      cmdExeArgs '("/c" "start" "")
      browse-url-generic-program  cmdExeBin
      browse-url-generic-args     cmdExeArgs
      browse-url-browser-function 'browse-url-generic)
  nil)

;; C-h for delete
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "M-DEL"))
(global-set-key (kbd "<f1>") 'help-command)

(use-package posframe
  :defer t
  :ensure t)

(use-package color-rg
  :load-path "site-lisp/color-rg/"
  :init
  (define-key global-map (kbd "C-x l") (make-sparse-keymap))
  :bind
  (("C-x l p" . color-rg-search-project)
   ("C-x l i" . color-rg-search-input)
   ("C-x l s" . color-rg-search-symbol)
   ("C-x l o" . lxs/search-org)
   )
  )

(use-package openwith
  :hook
  (after-init-hook . openwith-mode)
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" " okular" (file))))
  )

;; 自动保存文件
(use-package auto-save
  :load-path "site-lisp/auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-idle 2)
  )

;; (use-package which-key
;;   :defer t
;;   :disabled t
;;   :diminish
;;   :ensure t
;;   :config
;;   (which-key-mode)
;;   )

(provide 'init-basic)
