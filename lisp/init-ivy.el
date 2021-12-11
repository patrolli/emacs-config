(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; config for swiper
(use-package ivy
  :ensure t
  :bind
  (("\C-s" . 'swiper)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-c C-r" . 'ivy-resume)
   ("<f1> f" . 'counsel-describe-function)
   ("<f1> v" . 'counsel-describe-variable)
   ("C-c <f3>" . 'counsel-bm)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c i" . 'counsel-git)
   ("C-c j" . 'counsel-git-grep)
   ("C-c k" . 'counsel-ag))
  (:map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :config
  (ivy-mode 1)
  (setq ivy-wrap t)
  (setq ivy-use-vitual-buffers t)
  (use-package ivy-hydra
    :commands ivy-hydra-read-action
    :init (setq ivy-read-action-function #'ivy-hydra-read-action))
  (use-package ivy-yasnippet
    :bind ("C-c C-y" . ivy-yasnippet)))

;; smex 用于将 M-x 命令按使用频率排序
(use-package smex
  :ensure t)

(provide 'init-ivy)
