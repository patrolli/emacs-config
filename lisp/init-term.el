(use-package vterm
  :ensure t
  :defer t
  :bind (:map vterm-mode-map
	      ([f9] . shell-pop))
  :config
  (setq vterm-buffer-name-string nil)
  (defun my/vterm ()
  "Open a vterm by selecting a profile from a profile lists.
"
  (interactive)
   (let ((completion-ignore-case  t)
     (command (completing-read "Choose: " '("local"
                                         "aimax-ht -p 25051"; your profiles
                                         "aimax-ht -p 25532"
                                         "aimax-ht -p 25778"
					 "diggers3") nil t)))
  (if
      (not (string-equal command "local"))
      (with-current-buffer (vterm (concat "*" command "*"))
        (vterm-send-string (format "ssh %s" command))
         (vterm-send-return))
    (vterm (concat "*vterm-" command "*")))
  )))

(use-package shell-pop
  :bind ([f9] . shell-pop)
  :init (setq shell-pop-window-size 30
              shell-pop-shell-type
              (cond ((fboundp 'vterm) '("vterm" "*vterm*"  #'vterm))
                    (sys/win32p '("eshell" "*eshell*" #'eshell))
                    (t '("terminal" "*terminal*"
                         (lambda () (term shell-pop-term-shell)))))))

(provide 'init-term)
