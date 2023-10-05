(require 'init-const)
(require 'xah-utils)
(require 'personal)

(use-package pretty-hydra
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
       (propertize title 'face face)))))

;; (defun hydra-indicator-f (&optional state bcolor)
;;   (cond
;; 	((string-equal bcolor "red") (propertize state
;; 		 'face '((:background "red") (:foregrond "black"));; 'eyebrowse-mode-line-active
;; 		 'mouse-face 'mode-line-highlight))
;; 	((string-equal bcolor "green") (propertize state
;; 		 'face '((:background "green") (:foregrond "black"));; 'eyebrowse-mode-line-active
;; 		 'mouse-face 'mode-line-highlight))
;; 	((string-equal bcolor "blue") (propertize state
;; 		 'face '((:background "blue") (:foregrond "white"));; 'eyebrowse-mode-line-active
;; 		 'mouse-face 'mode-line-highlight))
;; 	)
;;     )

;; (defun hydra-refresh-mode-line (&optional state bcolor)
;;   (setq hydra-indicator (hydra-indicator-f state bcolor))
;;   (add-to-list 'mode-line-misc-info '((:eval hydra-indicator)))
;;   )

;; (setq hydra-indicator (hydra-indicator-f "N" "blue"))

;; (defun avy-goto-word-1-backward-in-line (char &optional arg)
;;   (interactive (list (read-char "char: " t)
;;                      current-prefix-arg))
;;   (avy-goto-word-1 char arg (point-at-bol) (point) nil))

;; (defun avy-goto-word-1-forward-in-line (char &optional arg)
;;   (interactive (list (read-char "char: " t)
;;                      current-prefix-arg))
;;   (avy-goto-word-1 char arg (point) (point-at-eol) nil))

;; (defun view-jump-brace ()
;;   "Jump to correspondence parenthesis"
;;   (interactive)
;;   (let ((c (following-char))
;;         (p (preceding-char)))
;;     (if (eq (char-syntax c) 40) (forward-list)
;;       (if (eq (char-syntax p) 41) (backward-list)
;;         (backward-up-list)))))

;; (setq cursor-in-non-selected-windows nil)

;; (defvar hydra-stack nil)

;; (defun hydra-push (expr)
;;   (push `(lambda () ,expr) hydra-stack))

;; (cl-defun hydra-pop (&rest)
;;   (interactive)
;;   (let ((x (pop hydra-stack)))
;;     (when x
;;       (funcall x))))

;; (defun next-user-buffer ()
;;   "Switch to the next user buffer.
;; “user buffer” is determined by `xah-user-buffer-q'.
;; URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
;; Version 2016-06-19"
;;   (interactive)
;;   (next-buffer)
;;   (let ((i 0))
;;     (while (< i 20)
;;       (if (not (xah-user-buffer-q))
;;           (progn (next-buffer)
;;                  (setq i (1+ i)))
;;         (progn (setq i 100))))))

;; (defun previous-user-buffer ()
;;   "Switch to the previous user buffer.
;; “user buffer” is determined by `xah-user-buffer-q'.
;; URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
;; Version 2016-06-19"
;;   (interactive)
;;   (previous-buffer)
;;   (let ((i 0))
;;     (while (< i 20)
;;       (if (not (xah-user-buffer-q))
;;           (progn (previous-buffer)
;;                  (setq i (1+ i)))
;;         (progn (setq i 100))))))

;; (defun hydra-normal-state-cursor()
;;   (setq-default cursor-type 'box)
;;   (set-cursor-color "green"))

;; (defun hydra-insert-state-cursor()
;;   (setq-default cursor-type 'bar)
;;   (set-cursor-color "red"))

;; (defun joe-scroll-other-window()
;;   (interactive)
;;   (scroll-other-window 1))
;; (defun joe-scroll-other-window-down ()
;;   (interactive)
;;   (scroll-other-window-down 1))
;; (defhydra hydra-window-size (:color red)
;;         "Windows size"
;;         ("h" shrink-window-horizontally "shrink horizontal")
;;         ("j" shrink-window "shrink vertical")
;;         ("k" enlarge-window "enlarge vertical")
;;         ("l" enlarge-window-horizontally "enlarge horizontal")
;; 	("q" hydra-pop "exit"))
;; (defhydra hydra-window-frame (:color red)
;;         "Frame"
;;         ("f" make-frame "new frame")
;;         ("x" delete-frame "delete frame")
;; 	("q" hydra-pop "exit"))
;; (defhydra hydra-window-scroll (:color red)
;;         "Scroll other window"
;;         ("n" joe-scroll-other-window "scroll")
;;         ("p" joe-scroll-other-window-down "scroll down")
;; 	("q" hydra-pop "exit"))

;; (defhydra hydra-reading
;;   (:pre (progn
;; 	  (setq hydra-is-helpful nil)
;; 	  (overwrite-mode -1)
;; 	  (hydra-refresh-mode-line "[N]" "green" )
;; 	  (hydra-normal-state-cursor)
;; 	       )
;;    :before-exit (progn
;; 		  (setq hydra-is-helpful t)
;; 		  (hydra-refresh-mode-line "[I]" "red")
;; 		  (hydra-insert-state-cursor)
;; 		  )
;; 	:foreign-keys run
;; 	:color amaranth
;; 	:hint nil)
;;   ""
;;   ("!" shell-pop)
;;   ("-" er/contract-region)
;;   ("=" er/expand-region)
;;   ("%" view-jump-brace)

;;   ;; org relevant
;;   ("p"  (progn
;; 	  (org-roam-hydra/body)
;; 	  (hydra-push '(hydra-reading/body))
;; 	  ) :color blue)
;;   ("M-l" (progn
;; 	   (worf-hydra/body)
;; 	   (hydra-push '(hydra-reading/body))
;; 	   ) :color blue)
;;   ;; ("/c" org-capture)
;;   ("dj" org-capture)
;;   ("/d" org-todo)
;;   ("/ss" org-schedule)
;;   ("/sd" org-deadline)
;;   ("/," org-priority)
;;   ("/a" org-agenda-list)
;;   ("/t" org-todo-list)
;;   ("/q" org-sidebar-toggle)
;;   ("/v" org-toggle-inline-images)
;;   ("/l" org-latex-preview)
;;   ("<SPC>'" (progn (if (org-src-edit-buffer-p)
;; 		       (org-edit-src-exit)
;; 		   (org-edit-special))))

;;   ;; ("." (progn (call-interactively 'avy-goto-char-timer)))
;;   (":" (progn (call-interactively 'eval-expression)))
;;   ;; ace-window
;;   (".." ace-window) ;; 为 ace-window 增加一个 rime 的断言
;;   (".x" delete-window) ;; do not chose when i want to close a window
;;   (".X" ace-delete-window)
;;   (".c" ace-swap-window)
;;   (".v" split-window-below)
;;   (".b" balance-windows)
;;   (".n" aw-flip-window)
;;   (".m" delete-other-windows)
;;   ;; ("q" delete-other-windows) ;; 如果没有其他要删除的窗口，那么就当作普通的 q
;;   ("q" toggle-maximize-window)
;;   ("Q" ace-delete-window)
;;   (".h" split-window-right)
;;   (".u" winner-undo)
;;   (".o" (progn
;; 	  (hydra-window-scroll/body)
;; 	  (hydra-push '(hydra-reading/body))
;; 	  ) :color blue)
;;   (".r" winner-redo)
;;   (".w" (progn
;; 	  (hydra-window-size/body)
;; 	  (hydra-push '(hydra-reading/body))
;; 	  ) :color blue)
;;   (".;" (progn
;; 	  (hydra-window-frame/body)
;; 	  (hydra-push '(hydra-reading/body))
;; 	  ) :color blue)
;;   (".f" toggle-frame-fullscreen)

;;   ;; (";j" (progn
;;   ;; 	  (hydra-projectile/body)
;;   ;; 	  (hydra-push '(hydra-reading/body))) :color blue)
;;   (";j" projectile-find-file)
;;   ("; M-j" projectile-find-file-other-window)
;;   (";f" counsel-file-jump)
;;   (";a" color-rg-search-project)
;;   ;; (";q" color-rg-search-input-in-current-file)
;;   (";o" lxs/search-org)
;;   (";e" eval-buffer)
;;   (";s" eval-last-sexp)
;;   (";b" xah-close-current-buffer)
;;   (";k" xah-new-empty-buffer)
;;   (";v" reveal-in-explorer)
;;   (";w" browse-html-of-org-buffer)
;;   ("; <SPC>" ivy-bibtex)
;;   (";g" ivy-resume)

;;   ;; bookmark
;;   (";i" (progn
;; 	  (hydra-bookmarks/body)
;; 	  (hydra-push '(hydra-reading/body))
;; 	  ) :color blue)
;;   (";y" counsel-yank-pop)
;;   (";r" counsel-rg)
;;   ("zj" xah-open-file-fast)
;;   ;; ("zl" counsel-bookmark)
;;   ("zh" (lambda () (interactive) (xs-set-bookmark (read-from-minibuffer "Name:"))))

;;   (";d" dired-jump)
;;   ;; (";j" persp-prev)
;;   (",t" treemacs :color blue)
;;   ;; (",j" xah-pop-local-mark-ring)
;;   (",j" backward-forward-previous-location)
;;   ;; (",k" pop-global-mark)
;;   (",k" backward-forward-next-location)
;;   (",s" counsel-imenu)
;;   (",a" lxs/insert-current-date-time)
;;   (",cj" org-roam-dailies-capture-today)
;;   ("C-c C-]" helm-bibtex :color blue)
;;   ("<mouse-1>" mouse-set-point :color blue)
;;   ("<mouse-3>" counsel-find-file)
;;   ("<" beginning-of-buffer)
;;   (">" end-of-buffer)
;;   ("@" avy-goto-line)
;;   ("A" (progn (beginning-of-line) (indent-according-to-mode)) :color blue)
;;   ("D" kill-line :color blue)
;;   ("E" end-of-line :color blue)
;;   ("F" (progn (call-interactively 'avy-goto-word-1-backward-in-line)))
;;   ;; ("H" (progn (ov-highlight/body) (hydra-push '(hydra-reading/body))) :color teal)
;;   ("I" (progn (forward-char 1)) :color blue)
;;   ("J" (progn (end-of-line) (newline-and-indent)) :color blue)
;;   ("K" (progn (beginning-of-line) (open-line 1) (indent-according-to-mode)) :color blue)
;;   ("S" my-counsel-grep-or-swiper)
;;   ("B" ibuffer)
;;   ;; ("<SPC>j" scroll-down)
;;   ;; ("<SPC>k" scroll-up)
;;   ("<SPC>k" xah-clean-whitespace)
;;   ("<SPC>o" xah-open-last-closed)
;;   ("<SPC>i" xah-open-recently-closed)
;;   ("<SPC>e" iedit-mode)
;;   ("<SPC>b" helm-mini)
;;   ;; ("<SPC>f" my-yapf-format-buffer)
;;   ("<SPC>f" counsel-dogears)
;;   ("<SPC>d" dogears-remember)
;;   ("<SPC>s" save-buffer)
;;   ("<SPC>h" eyebrowse-prev-window-config)
;;   ("<SPC>l" eyebrowse-next-window-config)
;;   ;; ("<SPC>g" eyebrowse-switch-to-window-config)
;;   ("<SPC>m" eyebrowse-create-window-config)
;;   ;; ("<SPC>q" replace-id-quick)
;;   ("<SPC><SPC>" self-insert-command)
;;   ("<SPC><tab>" xs-toggle-code-block)
;;   ("<SPC>," (progn (if (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
;; 		       (xs-prev-code-block))))
;;   ("<SPC>." (progn (if (eq (buffer-local-value 'major-mode (current-buffer)) 'org-mode)
;; 		       (xs-next-code-block))))
;;   ("V" (and (ignore-errors (other-window-for-scrolling) (scroll-other-window-down))))
;;   ("W" backward-word)
;;   ("X" (progn (kill-line 0)))
;;   ("a" (progn (xah-beginning-of-line-or-block)))
;;   ("b" ;; (progn (ibuffer) (swiper))
;;    (ivy-switch-buffer)
;;    )
;;   ;; ("c" (progn (overwrite-mode) (hydra-refresh-mode-line "[C]" "blue")) :color blue)
;;   ("dd" my-delete-whole-line) ;; delete without yank to kill-ring
;;   ("dl" my-kill-line)
;;   ("dw" my-delete-word)
;;   ("df" zap-to-char :color blue)
;;   ("de" my-delete-line :color blue)
;;   ("da" (progn (kill-line 0) (indent-according-to-mode)) :color blue)
;;   ;; ("dp" duplicate-line-or-region :color blue)
;;   ("dc" thing-cut-comment)
;;   ("ds" thing-cut-sexp)
;;   ;; ("dj" thing-cut-symbol)
;;   ;; ("e" end-of-line)
;;   ("e" xah-end-of-line-or-block)
;;   ("f" (progn (call-interactively 'avy-goto-word-1-forward-in-line)))
;;   ;; ("g" awesome-tab-counsel-switch-group)
;;   ;; ("g" keyboard-quit)
;;   ("g" eyebrowse-switch-to-window-config)
;;   ("G" end-of-buffer)
;;   ("h" backward-char)
;;   ("i" nil)
;;   ("j" next-line)
;;   ("k" previous-line)
;;   ("l" forward-char)
;;   ;; fast select
;;   ("mw" mark-word)
;;   ("ms" mark-sexp)
;;   ("mp" mark-paragraph)
;;   ("ml" xah-select-line)
;;   ("mm" set-mark-command)
;;   ("mj" er/expand-region)
;;   ("mk" er/contract-region)
;;   ("me" xah-extend-selection)
;;   ("mq" xah-select-text-in-quote)
;;   ("mb" xah-select-block)
;;   ("M-;" comment-line)
;;   ("s-;" comment-line)
;;   ("zk" helpful-key)
;;   ("zv" counsel-describe-variable)
;;   ("zf" counsel-describe-function)
;;   ("zg" keyboard-quit)
;;   ("zd" helpful-at-point)
;;   ;; ("n" (progn (ded/org-show-next-heading-tidily)))
;;   ("n" narrow-to-region)
;;   ("<SPC>n" widen)
;;   ;; ("o" ace-link) ;; future use for org-mode hydra
;;   ;; ("o" ace-window)
;;   ("o" newline-without-break-of-line)
;;   ;; ("p" (progn (ded/org-show-previous-heading-tidily)))
;;   ("r" undo-tree-redo)
;;   ;; ("s" my-counsel-grep-or-swiper)
;;   ;; ("s" swiper-isearch) ;TODO: 有点问题
;;   ("/" swiper)
;;   ("t" other-window)
;;   ("T" (other-window -1))
;;   ;; ("cc" avy-goto-char)
;;   ("cc" org-ctrl-c-ctrl-c)
;;   ("co" org-open-at-point)
;;   ("cj" avy-goto-char-2)
;;   ("cl" avy-goto-line)
;;   ("u" undo)
;;   ;; ("v" (save-excursion (and (ignore-errors (other-window-for-scrolling)) (scroll-other-window))))
;;   ("v" counsel-find-file)
;;   ("w" forward-word)
;;   ("x" delete-char)
;;   ("y" yank)
;;   ("M-j" nil)
;;   ("<escape>" nil)
;;   ("M-k" toggle-input-method)
;;   ("M-p" previous-user-buffer)
;;   ("M-n" next-user-buffer))

;; (bind-key "<escape>" 'hydra-reading/body)
;; (bind-key "M-j" 'hydra-reading/body)

(provide 'init-hydra)



