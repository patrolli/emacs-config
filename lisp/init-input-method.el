(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; (use-package pyim
;;   :ensure nil
;;   :disabled t
;;   :demand t
;;   :init
;;   ;; (setq pyim-dicts
;;   ;; 	'((:name "pyim-bigdict" :file "/mnt/c/Users/lixun/Downloads/pyim-bigdict.pyim")))  ;; 添加词库
;;   (setq pyim-dicts
;; 	'((:name "pyim-another-dict" :file "~/.emacs.d/pyim-dict/pyim-another-dict.pyim")))  ;; 添加词库
;;   (setq pyim-translate-trigger-char "@")
;;   (setq pyim-page-length 5)
;;   (setq pyim-page-tooltip 'posframe)
;;   (setq pyim-default-scheme 'xiaohe-shuangpin)
;;   :hook
;;   ('emacs-startup-hook . (lambda () (pyim-restart-1 t)))
;;   :bind
;;   (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
;;    ("C-;" . pyim-delete-word-from-personal-buffer))
;;   :config
;;   ;; (use-package pyim-basedict
;;   ;;   :ensure t
;;   ;;   :config (pyim-basedict-enable))
;;   (setq default-input-method "pyim")
;;   (setq pyim-dcache-backend 'pyim-dregcache)
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-isearch-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))
;;   (setq-default pyim-punctuation-half-width-functions
;; 	      '(pyim-probe-punctuation-line-beginning
;; 		pyim-probe-punctuation-after-punctuation)) ;; 自动半角全角切换
;;   ;; 使 ivy 支持拼音搜索
;;   (defun eh-ivy-cregexp (str)
;;     (let ((a (ivy--regex-plus str))
;;           (b (let ((case-fold-search nil))
;;                (pyim-cregexp-build str))))
;;       (if (and a (stringp a))
;;           (concat a "\\|" b)
;;         a)))
;;   (setq ivy-re-builders-alist
;;         '((t . eh-ivy-cregexp)))
;;   )

(use-package rime
  :defer t
  :custom
  (default-input-method "rime")
  :bind
  (:map rime-active-mode-map
   ("<tab>" . 'rime-inline-ascii)
   :map rime-mode-map
   ("C-`" . 'rime-send-keybinding)    ;; <---- 
   ;; ("M-j" . 'rime-force-enable)
   )
  :config
  (setq default-input-method "rime"
	rime-show-candidate 'posframe);;
  (setq rime-user-data-dir "~/.config/fcitx/rime")
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-space-after-cc-p
	  rime-predicate-hydra-p
          rime-predicate-punctuation-after-space-cc-p
	  rime-predicate-punctuation-after-ascii-p
          rime-predicate-prog-in-code-p
	  rime-predicate-in-code-string-p
	  rime-predicate-punctuation-line-begin-p
	  ;; rime-predicate-space-after-ascii-p
	  rime-predicate-ace-window-p
          rime-predicate-current-uppercase-letter-p
	  ;; rime-predicate-helm-mode-p
          ))
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))
  ;; (add-hook 'find-file-hook #'toggle-input-method) 
  )


(provide 'init-input-method)
