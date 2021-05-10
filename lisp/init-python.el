(require 'init-custom)

(use-package python
  :ensure nil
  ;; 和 auto-save 配合的话，format 的频率太高，现在还是手动 format.
  ;; :hook (python-mode . (lambda ()
  ;;                        (add-hook 'after-save-hook #'my-yapf-format-buffer t t)))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  (defun my-yapf-format-buffer ()
       (interactive)
       (when (and (executable-find "yapf") buffer-file-name)
         (call-process "yapf" nil nil nil "-i" "--style" "google" buffer-file-name)))
  )

(provide 'init-python)
