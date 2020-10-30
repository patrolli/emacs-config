 (when (>= emacs-major-version 24)   
   (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

 ;; cl - Common Lisp Extension
 (require 'cl)

 ;; Add Packages
 (defvar my/packages '(
		;; --- Auto-completion ---
		company
		company-lsp
		;; --- Better Editor ---
		hungry-delete
		smex
		swiper
		counsel
		smartparens
		popwin
		pyim
		super-save
		cnfonts
		neotree
		iedit
		expand-region
		lsp-mode
		flycheck
		use-package
		org-ref
		helm-org-rifle
		magit
		;; --- Major Mode ---
		js2-mode
		xahk-mode
		;; --- Minor Mode ---
		nodejs-repl
		exec-path-from-shell
		cdlatex
		undo-tree
		ace-jump-mode
		window-numbering
		openwith
		imenu-list
		ox-wk
		yasnippet
		;; --- Themes ---
		monokai-theme
		solarized-theme
		leuven-theme
		material-theme
		;; solarized-theme
		) "Default packages")

 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

(require 'pyim)
;;(require 'pyim-basedict)
;;(pyim-basedict-enable)
(setq default-input-method "pyim")
(setq pyim-default-scheme 'xiaohe-shuangpin)
(setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
;; 自动半角全角切换
(setq-default pyim-punctuation-half-width-functions
	      '(pyim-probe-punctuation-line-beginning
		pyim-probe-punctuation-after-punctuation))
;;(setq pyim-punctuation-translate-p '(no yes auto))
(setq pyim-page-tooltip 'posframe)
;; 添加词库
(setq pyim-dicts
      '((:name "pyim-bigdict" :file "/mnt/c/Users/lixun/Downloads/pyim-bigdict.pyim")))
(add-hook 'emacs-startup-hook
          #'(lambda () (pyim-restart-1 t)))

;; open global Company
(global-company-mode 1)
;; hungry-delete setting
(require 'hungry-delete)
(global-hungry-delete-mode)
;; config for swiper
(ivy-mode 1)
(setq ivy-use-vitual-buffers t)

;;smartparens
(require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)

;; config for popwin
(require 'popwin)
(popwin-mode 1)


;; cnfonts
(require 'cnfonts)
(cnfonts-enable)
;;(cnfonts-set-spacemacs-fallback-fonts)


;; super-save config for auto-save buffers
(setq auto-save-default nil)
(super-save-mode +1)

;; real-auto-save
;;(require 'real-auto-save)
;;(add-hook 'prog-mode-hook 'real-auto-save-mode)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; iedit 可以同时编辑多块区域
(require 'iedit)

;; expand-region 选择一块区域
(require 'expand-region)

;;
;;(setq lsp-keymap-prefix "s-l")
;;(require 'lsp-mode)
;; Enable LSP backend.
;;(push 'company-lsp company-backends)
;;(add-hook 'python-mode-hook #'lsp)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (c-mode . lsp)
	 (c++-mode .  lsp)
	 (python-mode . lsp))
  :commands lsp)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq lsp-diagnostic-package :none)
;; (setq lsp-diagnostic-package :none)

(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" "/home/lixunsong/anaconda3/envs"))

(use-package valign
        :load-path "~/.emacs.d/valign/"
        :config
        (add-hook 'org-mode-hook #'valign-mode))

;; flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode -1)

;; 切换窗口
(window-numbering-mode 1)

;; undo-tree, C-x u: show the undo-tree for current file
(require 'undo-tree)
(global-undo-tree-mode 1)

;; i want to open pdf with external app
(require 'openwith)
(openwith-mode t)
;; (setq openwith-associations '(("\\.pdf\\'" "qpdfview" (file))))
(setq openwith-associations '(("\\.pdf\\'" " ~/bin/FoxitReader" (file))))

;; imenu-list
(global-set-key (kbd "C-c l") 'imenu-list-minor-mode)
(setq imenu-list-focus-after-activation t)

;; wiki format converter from org
(require 'ox-wk)
;; yasnippet, code template
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(provide 'init-packages)
