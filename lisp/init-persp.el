(require 'init-custom)
(require 'init-funcs)

;; Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :diminish
  :defines (recentf-exclude ivy-ignore-buffers)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode)
         (persp-mode . persp-load-frame)
         (kill-emacs . persp-save-frame))
  :init (setq persp-keymap-prefix (kbd "C-c C-x s")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time 0)
  :bind
  (("C-x p" . hydra-persp/body))
  :config

  (defun my/persp-next ()
  "Switch to next perspective (to the right)."
  (interactive)
  (let* ((persp-list (persp-names-current-frame-fast-ordered))
         (persp-list-length (length persp-list))
         (only-perspective? (equal persp-list-length 1))
         (pos (cl-position (safe-persp-name (get-current-persp)) persp-list :test 'equal)))
    (cond
     ((null pos) nil)
     (only-perspective? nil)
     ((= pos (1- persp-list-length))
      (if persp-switch-wrap (persp-switch (nth 0 persp-list))))
     (t (persp-switch (nth (1+ pos) persp-list))))))

  (defun my/persp-prev ()
  "Switch to previous perspective (to the left)."
  (interactive)
  (let* ((persp-list (persp-names-current-frame-fast-ordered))
         (persp-list-length (length persp-list))
         (only-perspective? (equal persp-list-length 1))
         (pos (cl-position (safe-persp-name (get-current-persp)) persp-list :test 'equal)))
    (cond
     ((null pos) nil)
     (only-perspective? nil)
     ((= pos 0)
      (if persp-switch-wrap
          (persp-switch (nth (1- persp-list-length) persp-list))))
     (t (persp-switch (nth (1- pos) persp-list))))))
(persp-names-current-frame-fast-ordered)
  
  (defun hydra-perse-names ()
  (let ((names (persp-names-current-frame-fast-ordered))
        (current-name (safe-persp-name (get-current-persp)))
        (parts '())
        (count 1))
    (dolist (name names (s-join " | " (nreverse parts)))
      (cond ((eq name current-name)
             (push (format "[%d:%s]" count name) parts))
            (t
             (push (format "%d:%s" count name) parts)))
      (cl-incf count))))
  
  (defun my/persp-switch-to-n (n)
    (let ((names (persp-names-current-frame-fast-ordered))
        (count 1))
    (dolist (name names)
      (when (= count n)
        (persp-switch name))
      (cl-incf count))))
  (defun my/persp-switch-to-1 () (interactive) (my/persp-switch-to-n 1))
  (defun my/persp-switch-to-2 () (interactive) (my/persp-switch-to-n 2))
  (defun my/persp-switch-to-3 () (interactive) (my/persp-switch-to-n 3))
  (defun my/persp-switch-to-4 () (interactive) (my/persp-switch-to-n 4))
  (defun my/persp-switch-to-5 () (interactive) (my/persp-switch-to-n 5))
  (defun my/persp-switch-to-6 () (interactive) (my/persp-switch-to-n 6))
  (defun my/persp-switch-to-7 () (interactive) (my/persp-switch-to-n 7))
  (defun my/persp-switch-to-8 () (interactive) (my/persp-switch-to-n 8))
  (defun my/persp-switch-to-9 () (interactive) (my/persp-switch-to-n 9))
  (defun my/persp-switch-to-10 () (interactive) (my/persp-switch-to-n 10))
  
  (defun my/pick-layout ()
  "Switch to a new or existing layout."
  (interactive)
  (let* ((names (persp-names))
         (name (completing-read "Switch to layout: " names))
         (exists (persp-with-name-exists-p name)))
    (persp-switch name)
    (unless exists
      (switch-to-buffer "*scratch*"))))
  
  (defhydra hydra-persp (:hint nil :color red)
"
Layouts %s(hydra-perse-names)

^Navigation^      ^Selection^       ^Actions^        ^Buffers^
^-^---------------^-^---------------^-^--------------^-^------------
_n_: next         _l_: choose      _d_: delete      _a_: add buffer
_p_: previous     _L_: predefined  _r_: rename      _k_: remove buffer
^^                ^^               ^^               _K_: kill buffer
"
  ("q" hydra-pop "quit")
  ("a" persp-add-buffer :exit t)
  ("k" persp-remove-buffer :exit t)
  ("K" persp-kill-buffer :exit t)
  ("d" persp-kill)
  ("l" my/pick-layout :exit t)
  ("L" my/pick-predefined-layout :exit t)
  ("r" persp-rename :exit t)
  ("n" my/persp-next)
  ("p" my/persp-prev)
  ("1" my/persp-switch-to-1 )
  ("2" my/persp-switch-to-2 )
  ("3" my/persp-switch-to-3 )
  ("4" my/persp-switch-to-4 )
  ("5" my/persp-switch-to-5 )
  ("6" my/persp-switch-to-6 )
  ("7" my/persp-switch-to-7 )
  ("8" my/persp-switch-to-8 )
  ("9" my/persp-switch-to-9 )
  ("0" my/persp-switch-to-10))
  ;; Save and load frame parameters (size & position)
  (defvar persp-frame-file (expand-file-name "persp-frame" persp-save-dir)
    "File of saving frame parameters.")

  (defun persp-save-frame ()
    "Save the current frame parameters to file."
    (interactive)
    (when (and (display-graphic-p) lxs-restore-frame-geometry persp-mode)
      (condition-case error
          (with-temp-buffer
            (erase-buffer)
            (insert
             ";;; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
             ";;; This is the previous frame parameters.\n"
             ";;; Last generated " (current-time-string) ".\n"
             "(setq initial-frame-alist\n"
             (format "      '((top . %d)\n" (frame-parameter nil 'top))
             (format "        (left . %d)\n" (frame-parameter nil 'left))
             (format "        (width . %d)\n" (frame-parameter nil 'width))
             (format "        (height . %d)\n" (frame-parameter nil 'height))
             (format "        (fullscreen . %s)))\n" (frame-parameter nil 'fullscreen)))
            (when (file-writable-p persp-frame-file)
              (write-file persp-frame-file)))
        (error
         (warn "persp frame: %s" (error-message-string error))))))

  (defun persp-load-frame ()
    "Load frame with the previous frame's geometry."
    (interactive)
    (when (and (display-graphic-p) lxs-restore-frame-geometry persp-mode)
      (when (file-readable-p persp-frame-file)
        (condition-case error
            (progn
              (load persp-frame-file)
              ;; Handle multiple monitors gracefully
              (when (or (>= (eval (frame-parameter nil 'left)) (display-pixel-width))
                        (>= (eval (frame-parameter nil 'top)) (display-pixel-height)))
                (set-frame-parameter nil 'left 0)
                (set-frame-parameter nil 'top 0)))
          (error
           (warn "persp frame: %s" (error-message-string error)))))))

  (with-no-warnings
    ;; Don't save if the sate is not loaded
    (defvar persp-state-loaded nil
      "Whether the state is loaded.")

    (defun my-persp-after-load-state (&rest _)
      (setq persp-state-loaded t))
    (advice-add #'persp-load-state-from-file :after #'my-persp-after-load-state)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (add-hook 'find-file-hook #'my-persp-after-load-state)))

    (defun my-persp-asave-on-exit (fn &optional interactive-query)
      (if persp-state-loaded
          (funcall fn interactive-query)
        t))
    (advice-add #'persp-asave-on-exit :around #'my-persp-asave-on-exit))

  ;; Don't save dead or temporary buffers
  (add-to-list 'persp-filter-save-buffers-functions
               (lambda (b)
                 "Ignore dead buffers."
                 (not (buffer-live-p b))))
  (add-to-list 'persp-filter-save-buffers-functions
               (lambda (b)
                 "Ignore temporary buffers."
                 (let ((bname (file-name-nondirectory (buffer-name b))))
                   (or (string-prefix-p ".newsrc" bname)
                       (string-prefix-p "magit" bname)
                       (string-prefix-p "Pfuture-Callback" bname)
                       (string-prefix-p "treemacs-persist" bname)
                       (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                       (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)
                       (eq (buffer-local-value 'major-mode b) 'erc-mode)
                       (eq (buffer-local-value 'major-mode b) 'rcirc-mode)
                       (eq (buffer-local-value 'major-mode b) 'nov-mode)
                       (eq (buffer-local-value 'major-mode b) 'vterm-mode)
		       ))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; TODO: 针对单一模式的 buffer, 自动将其归入到一个 persp 中
  ;; (with-eval-after-load "persp-mode-autoload"
  ;;     (with-eval-after-load "vterm"
  ;;       (persp-def-auto-persp "vterm"
  ;;         :parameters '((dont-save-to-file . t))
  ;;         :mode 'vterm-mode
  ;;         :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
  ;;                    (persp-add-buffer-on-find-file nil)
  ;;                    persp-add-buffer-on-after-change-major-mode)
  ;;         :hooks '(after-switch-to-buffer-functions)
  ;;         :switch 'window)))

  ;; Ivy Integraticon
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 #'(lambda (b)
                     (when persp-mode
                       (let ((persp (get-current-persp)))
                         (if persp
                             (not (persp-contain-buffer-p b persp))
                           nil))))))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Shell integration
  (persp-def-buffer-save/load
   :mode 'shell-mode :tag-symbol 'def-shell-buffer
   :mode-restore-function (lambda (_) (shell))
   :save-vars '(major-mode default-directory)))

;; Projectile integration
(use-package persp-mode-projectile-bridge
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives)
  :hook ((persp-mode . persp-mode-projectile-bridge-mode)
         (persp-mode-projectile-bridge-mode
          .
          (lambda ()
            (if persp-mode-projectile-bridge-mode
                (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
              (persp-mode-projectile-bridge-kill-perspectives)))))
  :init (setq persp-mode-projectile-bridge-persp-name-prefix "<p>")
  :config
  (with-no-warnings
    ;; HACK: Allow saving to files
    (defun my-persp-mode-projectile-bridge-add-new-persp (name)
      (let ((persp (persp-get-by-name name *persp-hash* :nil)))
        (if (eq :nil persp)
            (prog1
                (setq persp (persp-add-new name))
              (when persp
                (set-persp-parameter 'persp-mode-projectile-bridge t persp)
                (persp-add-buffer (projectile-project-buffers)
                                  persp nil nil)))
          persp)))
    (advice-add #'persp-mode-projectile-bridge-add-new-persp
                :override #'my-persp-mode-projectile-bridge-add-new-persp)

    ;; HACK: Switch to buffer after switching perspective
    (defun my-persp-mode-projectile-bridge-hook-switch (&rest _args)
      (let* ((buf (current-buffer))
             (persp (persp-mode-projectile-bridge-find-perspective-for-buffer buf)))
        (when persp
          (when (buffer-live-p
                 persp-mode-projectile-bridge-before-switch-selected-window-buffer)
            (let ((win (selected-window)))
              (unless (eq (window-buffer win)
                          persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                (set-window-buffer
                 win persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer nil))))
          (persp-frame-switch (persp-name persp))

          (when (buffer-live-p buf)
            (switch-to-buffer buf)))))
    (advice-add #'persp-mode-projectile-bridge-hook-switch
                :override #'my-persp-mode-projectile-bridge-hook-switch)))

(provide 'init-persp)
