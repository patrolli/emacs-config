(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

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
	rime-show-candidate 'posframe)
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
            ;; :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))
  ;; (add-hook 'find-file-hook #'toggle-input-method) 
  )
(global-set-key (kbd "M-k") 'toggle-input-method) 
(provide 'init-input-method)
