(setq dired-recursive-copies 'always)

;; 延迟加载dired，节省emacs启动时间
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-c C-o") 'xah-open-in-xternal-app-from-wsl))
;; 打开当前目录的dired mode，快捷键C－c C－j
(require 'dired-x)
(setq dired-dwim-target 1)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
(put 'dired-find-alternate-file 'disabled nil)

;; Shows icons
(use-package all-the-icons-dired
  :ensure t
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; FIXME: Refresh after creating or renaming the files/directories.
  ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/34.
  (with-no-warnings
    (advice-add #'dired-do-create-files :around #'all-the-icons-dired--refresh-advice)
    (advice-add #'dired-create-directory :around #'all-the-icons-dired--refresh-advice)
    (advice-add #'wdired-abort-changes :around #'all-the-icons-dired--refresh-advice))

  (with-no-warnings
    (defun my-all-the-icons-dired--refresh ()
      "Display the icons of files in a dired buffer."
      (all-the-icons-dired--remove-all-overlays)
      ;; NOTE: don't display icons it too many items
      (if (<= (count-lines (point-min) (point-max)) 1000)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((file (file-local-name (dired-get-filename 'relative 'noerror))))
                  (when file
                    (let ((icon (if (file-directory-p file)
                                    (all-the-icons-icon-for-dir file
                                                                :face 'all-the-icons-dired-dir-face
                                                                :height 0.9
                                                                :v-adjust all-the-icons-dired-v-adjust)
                                  (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (point) "  \t")
                        (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
              (forward-line 1)))
        (message "Not display icons because of too many items.")))
    (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)

(provide 'init-dired)
