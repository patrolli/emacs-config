(require 'init-custom)

(use-package python
  :ensure nil
  ;; 和 auto-save 配合的话，format 的频率太高，现在还是手动 format.
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil))

(provide 'init-python)
