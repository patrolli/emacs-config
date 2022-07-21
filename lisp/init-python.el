(require 'init-custom)

(use-package python
  :ensure nil
  ;; 和 auto-save 配合的话，format 的频率太高，现在还是手动 format.
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-interpreter "python")
  )

(use-package python-black
  :demand t
  :after python
  :config
  (setq python-black-command "C:\\Users\\xunsong.li\\AppData\\Roaming\\Python\\Python310\\Scripts\\black.exe")
  )

(provide 'init-python)
