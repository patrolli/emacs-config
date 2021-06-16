(require 'init-const)
(require 'xah-utils)
(require 'personal)

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :ensure t
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
 _j_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear 
 _ff_: file dwim       _g_: color-rg          _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur      _.k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("j" projectile-find-file)
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
  ("q"   hydra-pop "cancel" :color blue))

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

(defhydra hydra-bookmarks (:color teal :hint none)
  "
Bookmarks (_q_uit)
^bookmarks^         ^bm bookmark^    ^hl todo^
^--------^-------   ^-----^------
_a_dd bookmark      _j_ next bm      _h_ next TODO
_n_ame bookmark     _k_ prev bm      _g_ prev TODO
_l_ist bookmark     _s_ toggle bm    sh_o_w TODO
^^                  _f_ show bm      _i_nsert TODO
"
  ;; bookmarks
  ("q" hydra-pop )
  ("a" hydra--set-bookmark )
  ("n" (lambda () (interactive) (hydra--set-bookmark (read-from-minibuffer "Name:"))) )
  ("l" my-bookmark-bmenu-list)
  ;; bm bookmark
  ("j" bm-next :exit nil)
  ("k" bm-previous :exit nil)
  ("s" bm-toggle :exit nil)
  ("f" counsel-bm :exit nil)
  ;; hl-todo
  ("h" hl-todo-next :color red)
  ("g" hl-todo-previous :color red)	
  ("o" hl-todo-occur)
  ("i" hl-todo-insert)
  )			

;; (setq hydra-mode-line-format 'after)
;; (defun hydra-refresh-mode-line (&optional state)
;;   "Refresh mode line tag."
;;   (when (listp mode-line-format)
;;     (setq hydra-mode-line-tag (propertize state
;; 					  'face 'eyebrowse-mode-line-active
;; 					  'mouse-face 'mode-line-highlight)) ;; 自己定义的 hydra mode-line 样式
;;     ;; refresh mode line data structure
;;     ;; first remove hydra from mode-line
;;     (setq mode-line-format (delq 'hydra-mode-line-tag mode-line-format)) ;; 先去掉 hydra-mode-line
;;     (let ((mlpos mode-line-format)
;;           pred which where)
;;       ;; determine before/after which symbol the tag should be placed
;;       (cond
;;        ((eq hydra-mode-line-format 'before)
;;         (setq where 'after which 'mode-line-position))
;;        ((eq hydra-mode-line-format 'after)
;;         (setq where 'after which 'mode-line-modes))
;;        ((consp hydra-mode-line-format)
;;         (setq where (car hydra-mode-line-format)
;;               which (cdr hydra-mode-line-format))))
;;       ;; find the cons-cell of the symbol before/after which the tag
;;       ;; should be placed
;;       (while (and mlpos
;;                   (let ((sym (or (car-safe (car mlpos)) (car mlpos))))
;;                     (not (eq which sym))))
;;         (setq pred mlpos
;;               mlpos (cdr mlpos)))
;;       ;; put hydra tag at the right position in the mode line
;;       (cond
;;        ((not mlpos)) ;; position not found, so do not add the tag
;;        ((eq where 'before)
;;         (if pred
;;             (setcdr pred (cons 'hydra-mode-line-tag mlpos))
;;           (setq mode-line-format
;;                 (cons 'hydra-mode-line-tag mode-line-format))))
;;        ((eq where 'after)
;;         (setcdr mlpos (cons 'hydra-mode-line-tag (cdr mlpos)))))
;;       (force-mode-line-update))))

(defun hydra-indicator-f (&optional state bcolor)
  (cond 
	((string-equal bcolor "red") (propertize state
		 'face '((:background "red") (:foregrond "black"));; 'eyebrowse-mode-line-active 
		 'mouse-face 'mode-line-highlight))
	((string-equal bcolor "green") (propertize state
		 'face '((:background "green") (:foregrond "black"));; 'eyebrowse-mode-line-active 
		 'mouse-face 'mode-line-highlight))
	((string-equal bcolor "blue") (propertize state
		 'face '((:background "blue") (:foregrond "white"));; 'eyebrowse-mode-line-active 
		 'mouse-face 'mode-line-highlight))
	)
    )

(defun hydra-refresh-mode-line (&optional state bcolor)
  (setq hydra-indicator (hydra-indicator-f state bcolor))
  (add-to-list 'mode-line-misc-info '((:eval hydra-indicator)))
  )

(setq hydra-indicator (hydra-indicator-f "N" "blue"))

(defun avy-goto-word-1-backward-in-line (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-word-1 char arg (point-at-bol) (point) nil))

(defun avy-goto-word-1-forward-in-line (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-word-1 char arg (point) (point-at-eol) nil))

(defun view-jump-brace ()
  "Jump to correspondence parenthesis"
  (interactive)
  (let ((c (following-char))
        (p (preceding-char)))
    (if (eq (char-syntax c) 40) (forward-list)
      (if (eq (char-syntax p) 41) (backward-list)
        (backward-up-list)))))

(setq cursor-in-non-selected-windows nil)

(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

(defun ded/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (recenter-top-bottom)
    (show-children)
    (recenter-top-bottom)))

(defun ded/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (recenter-top-bottom)
    (show-children)
    (recenter-top-bottom)))

(defun next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun hydra-normal-state-cursor()
  (setq-default cursor-type 'box)
  (set-cursor-color "green"))

(defun hydra-insert-state-cursor()
  (setq-default cursor-type 'bar)
  (set-cursor-color "red"))

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

  (defun gcm-scroll-down ()
      (interactive)
      (scroll-up 1))
(defun gcm-scroll-up ()
      (interactive)
      (scroll-down 1))
(defun joe-scroll-other-window()
  (interactive)
  (scroll-other-window 1))
(defun joe-scroll-other-window-down ()
  (interactive)
  (scroll-other-window-down 1))
(defhydra hydra-window-size (:color red)
        "Windows size"
        ("h" shrink-window-horizontally "shrink horizontal")
        ("j" shrink-window "shrink vertical")
        ("k" enlarge-window "enlarge vertical")
        ("l" enlarge-window-horizontally "enlarge horizontal")
	("q" hydra-pop "exit"))
(defhydra hydra-window-frame (:color red)
        "Frame"
        ("f" make-frame "new frame")
        ("x" delete-frame "delete frame")
	("q" hydra-pop "exit"))
(defhydra hydra-window-scroll (:color red)
        "Scroll other window"
        ("n" joe-scroll-other-window "scroll")
        ("p" joe-scroll-other-window-down "scroll down")
	("q" hydra-pop "exit"))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.
• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, beginning of line means visual line.
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04 2021-03-16"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
            (progn
              (skip-chars-backward "\n\t ")
              ;; (forward-char )
              )
          (goto-char (point-min)))
      (if visual-line-mode
          (beginning-of-visual-line)
        (progn
          (back-to-indentation)
          (when (eq $p (point))
            (beginning-of-line)))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.
• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, end of line means visual line.
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04 2021-03-16"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (re-search-forward "\n[\t\n ]*\n+" nil "move" )
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))))

(defhydra hydra-reading
  (:pre (progn (setq hydra-is-helpful nil)(overwrite-mode -1) (hydra-refresh-mode-line "[N]" "green" ) (hydra-normal-state-cursor) 
	       )
   :before-exit (progn (setq hydra-is-helpful t) (hydra-refresh-mode-line "[I]" "red")
		       (hydra-insert-state-cursor)
			    )
	:foreign-keys run
	:color amaranth
	:hint nil)
  ""
  ("!" shell-pop)
  ("-" er/contract-region)
  ("=" er/expand-region)
  ("%" view-jump-brace)

  ;; org relevant
  ("/r"  (progn
	  (org-roam-hydra/body)
	  (hydra-push '(hydra-reading/body))	  
	  ) :color blue)
  ("/c" org-capture)
  ("<SPC>'" (progn (if (org-src-edit-buffer-p)
		       (org-edit-src-exit)
		   (org-edit-special))))
   
  ;; ("." (progn (call-interactively 'avy-goto-char-timer)))
  (":" (progn (call-interactively 'eval-expression)))
  ;; ace-window
  (".." ace-window) ;; 为 ace-window 增加一个 rime 的断言
  (".x" delete-window) ;; do not chose when i want to close a window
  (".X" ace-delete-window)
  (".c" ace-swap-window)
  (".v" split-window-below)
  (".b" balance-windows)
  (".n" aw-flip-window)
  (".m" delete-other-windows)
  (".h" split-window-right)
  (".u" winner-undo)
  (".o" (progn
	  (hydra-window-scroll/body)
	  (hydra-push '(hydra-reading/body))	  
	  ) :color blue)
  (".r" winner-redo)
  (".w" (progn
	  (hydra-window-size/body)
	  (hydra-push '(hydra-reading/body))	  
	  ) :color blue)
  (".;" (progn
	  (hydra-window-frame/body)
	  (hydra-push '(hydra-reading/body))	  
	  ) :color blue)
  (".f" toggle-frame-fullscreen)
  
  ;; (";j" (progn
  ;; 	  (hydra-projectile/body)
  ;; 	  (hydra-push '(hydra-reading/body))) :color blue)
  (";j" projectile-find-file)
  ("; M-j" projectile-find-file-other-window)
  (";f" counsel-file-jump)
  (";a" color-rg-search-project)
  (";q" color-rg-search-input-in-current-file)
  (";o" lxs/search-org)
  (";e" eval-buffer)
  (";s" eval-last-sexp)
  (";b" xah-close-current-buffer)
  (";k" xah-new-empty-buffer)
  
  ;; bookmark
  (";i" (progn
	  (hydra-bookmarks/body)
	  (hydra-push '(hydra-reading/body))	  
	  ) :color blue)
  (";y" counsel-yank-pop "show yank history")
  (";r" counsel-rg)
  ("zl" my-bookmark-bmenu-list)
  ("zj" xah-open-file-fast)
  ;; ("zl" counsel-bookmark)
  ("zh" (lambda () (interactive) (hydra--set-bookmark (read-from-minibuffer "Name:"))))
  
  (";d" dired-jump)
  ;; (";j" persp-prev)
  (",t" treemacs :color blue)
  (",j" xah-pop-local-mark-ring)
  (",a" lxs/switch-to-agenda)
  (",cj" org-roam-dailies-capture-today)
  (",m" pop-global-mark)
  ("C-c C-]" helm-bibtex :color blue)
  ("<mouse-1>" mouse-set-point :color blue)
  ("<mouse-3>" counsel-find-file)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("@" avy-goto-line)
  ("A" (progn (beginning-of-line) (indent-according-to-mode)) :color blue)
  ("D" kill-line :color blue)
  ("E" end-of-line :color blue)
  ("F" (progn (call-interactively 'avy-goto-word-1-backward-in-line)))
  ;; ("H" (progn (ov-highlight/body) (hydra-push '(hydra-reading/body))) :color teal)
  ("I" (progn (forward-char 1)) :color blue)
  ("J" (progn (end-of-line) (newline-and-indent)) :color blue)
  ("K" (progn (beginning-of-line) (open-line 1) (indent-according-to-mode)) :color blue)
  ("N" next-user-buffer)
  ("P" previous-user-buffer)
  ("S" swiper-all)
  ("B" ibuffer) 
  ("<SPC>j" scroll-down)
  ("<SPC>k" scroll-up)
  ("<SPC>o" xah-open-last-closed)
  ("<SPC>i" xah-open-recently-closed)
  ("<SPC>e" iedit-mode)
  ("<SPC>b" helm-mini)
  ;; ("T" org-babel-tangle)
  ("V" (and (ignore-errors (other-window-for-scrolling) (scroll-other-window-down))))
  ("W" backward-word)
  ("X" (progn (kill-line 0)))
  ("[" beginning-of-defun)	     
  ("]" end-of-defun) 
  ;; ("a" (progn (beginning-of-line) (indent-according-to-mode)))
  ("a" (progn (xah-beginning-of-line-or-block)))
  ("b" ;; (progn (ibuffer) (swiper))
   (ivy-switch-buffer)
   )
  ;; ("c" (progn (overwrite-mode) (hydra-refresh-mode-line "[C]" "blue")) :color blue)
  ("dd" my-delete-whole-line) ;; delete without yank to kill-ring
  ("dw" my-delete-word)
  ("df" zap-to-char :color blue)
  ("de" my-delete-line :color blue)
  ("da" (progn (kill-line 0) (indent-according-to-mode)) :color blue)
  ;; ("dp" duplicate-line-or-region :color blue)
  ("dc" thing-cut-comment)
  ("ds" thing-cut-sexp)
  ("dj" thing-cut-symbol)
  ;; ("e" end-of-line)
  ("e" xah-end-of-line-or-block)
  ("f" (progn (call-interactively 'avy-goto-word-1-forward-in-line)))
  ("g" google-this)
  ("G" go-translate)
  ("h" backward-char)
  ("i" nil)
  ("j" next-line)
  ("k" previous-line)
  ("l" forward-char)
  ;; fast select
  ("mw" mark-word)
  ("ms" mark-sexp)
  ("mp" mark-paragraph)
  ("ml" xah-select-line)
  ("mm" set-mark-command)
  ("mj" er/expand-region)
  ("mk" er/contract-region)
  ("me" xah-extend-selection)
  ("mq" xah-select-text-in-quote)
  ("mb" xah-select-block)
  
  ("M-;" comment-line)
  ("zk" helpful-key)
  ("zv" counsel-describe-variable)
  ("zf" counsel-describe-function)
  ("zg" keyboard-quit)
  ("zd" helpful-at-point)
  ;; ("n" (progn (ded/org-show-next-heading-tidily)))
  ("n" narrow-to-region)
  ("<SPC>n" widen)
  ;; ("o" ace-link) ;; future use for org-mode hydra
  ("o" ace-window)
  ("p" (progn (ded/org-show-previous-heading-tidily)))
  ("r" undo-tree-redo)
  ("s" counsel-grep)
  ("t" other-window)
  ("T" (other-window -1))
  ("cc" avy-goto-char)
  ("cj" avy-goto-char-2)
  ("cl" avy-goto-line)
  ("u" undo)
  ;; ("v" (save-excursion (and (ignore-errors (other-window-for-scrolling)) (scroll-other-window))))
  ("v" counsel-find-file)
  ("w" forward-word)
  ("x" delete-char)
  ("y" yank)
  ("<escape>" nil)
  ("M-j" nil)
  ("M-k" toggle-input-method))

(bind-key "<escape>" 'hydra-reading/body)
(bind-key "M-j" 'hydra-reading/body)	

;; (add-hook 'find-file-hook '(lambda () (progn (hydra-pop) (hydra-reading/body))))

(provide 'init-hydra)



