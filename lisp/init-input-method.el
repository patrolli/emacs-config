(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package pyim
  :init
  (setq default-input-method "pyim")
  (setq pyim-indicator-list nil)
  ;; (pyim-indicator-stop-daemon)
  :bind
  ("M-k" . pyim-convert-string-at-point)
  ;; :hook
  ;; (after-init . pyim-indicator-stop-daemon)
  :config
  ;; (global-set-key (kbd "M-k") 'pyim-convert-string-at-point)
  (pyim-indicator-stop-daemon)
  (pyim-default-scheme 'xiaohe-shuangpin)
  (if (posframe-workable-p)
    (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-tooltip 'popup))
  (setq pyim-page-length 7)
  (setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))
  (setq pyim-dicts
	`((:name "sougou_base" :file ,(concat lxs-home-dir".emacs.d/pyim/sougou_base.pyim")))
	pyim-enable-shortcode nil)
  ;; ivy 拼音搜索
  (defun eh-ivy-cregexp (str)
    (let ((a (ivy--regex-plus str))
          (b (let ((case-fold-search nil))
               (pyim-cregexp-build str))))
      (if (and a (stringp a))
          (concat a "\\|" b)
        a)))
  (setq ivy-re-builders-alist
        '((t . eh-ivy-cregexp))))

;; (global-set-key (kbd "M-k") 'toggle-input-method) 
(provide 'init-input-method)
