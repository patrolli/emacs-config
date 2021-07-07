(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; 由于 rime 不能记忆用户自造词，所以切换回了 pyim
(use-package rime  
  :defer t
  :disabled t
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
	rime-show-candidate 'posframe)
  ;; (setq rime-librime-root "~/.emacs.d/librime/dist")
  (setq rime-user-data-dir "~/.emacs.d/rime/")
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
            ;; :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))
  ;; (add-hook 'find-file-hook #'toggle-input-method
  )

(use-package pyim
  :init
  (setq default-input-method "pyim")
  (setq pyim-indicator #'ignore)
  :bind
  ("M-k" . pyim-convert-string-at-point)
  :config
  ;; (global-set-key (kbd "M-k") 'pyim-convert-string-at-point)
  (pyim-default-scheme 'xiaohe-shuangpin)
  (if (posframe-workable-p)
    (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-tooltip 'popup))
  (setq pyim-page-length 5)
  (setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))
  (setq pyim-dicts
	'((:name "sougou_base" :file "~/.emacs.d/pyim/sougou_base.pyim"))
	pyim-enable-shortcode nil)
  (pyim-indicator-stop-daemon)
  
  )

;; (global-set-key (kbd "M-k") 'toggle-input-method) 
(provide 'init-input-method)
