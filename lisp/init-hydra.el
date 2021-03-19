(require 'init-const)

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on" :v-adjust -0.1)
                                        :color amaranth :quit-key "q")
      ("Basic"
       (("n" (if (fboundp 'display-line-numbers-mode)
                 (display-line-numbers-mode (if display-line-numbers-mode -1 1))
               (global-linum-mode (if global-linum-mode -1 1)))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode) global-linum-mode))
        ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("i" display-time-mode "time" :toggle t)
        ("m" doom-modeline-mode "modern mode-line" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" highlight-indent-guides-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Coding"
       (("f" flycheck-mode "flycheck" :toggle t)
        ("F" flymake-mode "flymake" :toggle t)
        ("o" origami-mode "folding" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
	;; ("S" my/toggle-auto-save "auto save" :toggle t)) ;; TODO: 加入自动保存的选项
       "Version Control"
       (("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))))))

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
 _.f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: color-rg          _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur      _.k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  (".f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ;; ("g"   ggtags-update-tags)
  ;; (".g" ggtags-update-tags)
  ("g" lxs/color-rg-search-specify-project)  ;; TODO: 加强为搜索指定 project 中的内容
  ("G" color-rg-search-project)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  (".k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  (".p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

(defun lxs/color-rg-search-specify-project ()
  (interactive)
  (let ((projects (projectile-relevant-known-projects)))
    (message "%s" projects)
    (let ((chosen-project (completing-read "Choose: " projects)))
      (color-rg-search-input (color-rg-pointer-string) chosen-project)
      )
    )
  )

(global-set-key (kbd "C-c p") 'hydra-projectile/body)

(defun hydra--view-bookmarks ()
  (interactive)
  (hydra-bookmarks/body)
  (helm-bookmarks))

(defun hydra--set-bookmark (&optional name)
  (interactive)
  (let ((filename (or name (format "%s:%s" (buffer-name) (line-number-at-pos)))))
    (bookmark-set filename)))

(defhydra hydra-bookmarks (:color blue)
  "bookmarks"
  ("q" nil "quit")
  ("a" hydra--set-bookmark "here")
  ("n" (lambda () (interactive) (hydra--set-bookmark (read-from-minibuffer "Name:"))) "named")
  ("l" counsel-bookmark "list"))


(provide 'init-hydra)



