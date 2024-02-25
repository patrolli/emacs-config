(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package pyim
  :init
  (setq default-input-method "pyim")
  (setq pyim-indicator-list nil)
  :bind
  ("M-k" . pyim-convert-string-at-point)
  ("s-k" . pyim-convert-string-at-point)
  :config
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

;; {{ enable evil-escape when using pyim
(with-eval-after-load 'evil-escape
  (defun pyim-plus-self-insert-command (orig-func)
    (interactive "*")
    (let ((first-key (elt evil-escape-key-sequence 0))
          (second-key (elt evil-escape-key-sequence 1)))
      (if (and (local-variable-p 'last-event-time)
               (floatp last-event-time)
               (< (- (float-time) last-event-time) evil-escape-delay))
          (set (make-local-variable 'temp-evil-escape-mode) t)
        (set (make-local-variable 'temp-evil-escape-mode) nil))
      (if (and temp-evil-escape-mode
               (string-prefix-p (char-to-string first-key) (reverse (pyim-entered-get 'point-before)))
               (equal last-command-event second-key))
          (progn
            (push last-command-event unread-command-events)
            ;; (pyim-process-outcome-handle 'pyim-entered)
            (push (pyim-entered-get 'point-before) pyim-outcome--history)
            (pyim-process-terminate))
        (progn
          (call-interactively orig-func)
          (set (make-local-variable 'last-event-time) (float-time))))))
  (advice-add 'pyim-self-insert-command :around #'pyim-plus-self-insert-command)
)
;; }}

(provide 'init-input-method)
