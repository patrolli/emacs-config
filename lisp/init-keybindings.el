(global-set-key (kbd "C-.") 'toggle-input-method)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "C-c <f3>") 'counsel-bm)

;; help in emacs
(global-set-key (kbd "<f1> C-f") 'find-function)
(global-set-key (kbd "<f1> C-v") 'find-variable)
(global-set-key (kbd "<f1> C-k") 'find-function-on-key)

;; C-h for delete
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-M-h") (kbd "M-DEL"))
(global-set-key (kbd "<f1>") 'help-command)

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-c p f") 'counsel-git)

;; global buffer indent
(global-key-binding (kbd "C-M-\\") 'indent-region-or-buffer)

;; bind hippie completation
(global-set-key (kbd "s-/") 'hippie-expand)

;; bind pyim probe
(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

;; short key mapping
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; Shifu
					    ("8im" "/mnt/c/Users/lixun/Documents/org/static")
					   ))

;; org-mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;;(global-set-key (kbd "C-c T") 'org-table-toggle-column-width)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c C-l") 'org-insert-link)

;; 用C-n, C-p来上下移动company补全的内容
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))




;; expand-region keybinding
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; ag, global keyword searching
(global-set-key (kbd "C-c C-x p s") 'helm-do-ag-project-root)

;; org-ref
(global-set-key (kbd "C-c s") 'dblp-lookup)
(global-set-key (kbd "C-c ]") 'org-ref-helm-insert-cite-link)

(provide 'init-keybindings)

