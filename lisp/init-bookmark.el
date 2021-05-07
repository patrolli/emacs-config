(require 'init-funcs)
(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-restore-repository-on-load nil)
  ;; :hook
  ;; ((after-save-hook . bm-buffer-save)
  ;;  (kill-buffer-hook . bm-buffer-save)
  ;;  (kill-emacs-hook . (lambda nil
  ;;                              (bm-buffer-save-all)
  ;;                              (bm-repository-save)))
  ;;  (find-file-hooks . bm-buffer-restore)
  ;;  (after-revert-hook . bm-buffer-restore)
  ;;  (vc-before-checkin-hook bm-buffer-save))
  :config
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file (concat user-emacs-directory "bm-repository"))
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)


  
  (defun bm-counsel-get-list (bookmark-overlays)
  (-map (lambda (bm)
          (with-current-buffer (overlay-buffer bm)
            (let* ((line (replace-regexp-in-string "\n$" "" (buffer-substring (overlay-start bm)
                                                                              (overlay-end bm))))
                   ;; line numbers start on 1
                   (line-num (+ 1 (count-lines (point-min) (overlay-start bm))))
                   (name (format "%s:%d - %s" (buffer-name) line-num line)))

              `(,name . ,bm))))
        bookmark-overlays))

  (defun counsel-bm-update-input ()
  "Update fn for counsel-bm."
  (with-ivy-window
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((chosen (ivy-state-current ivy-last))
             (bookmark (gethash chosen bm-hash-table)))
        (if chosen
            (save-restriction
              (with-ivy-window
                (switch-to-buffer (overlay-buffer bookmark))
                (bm-goto bookmark)))
          nil)))))

  (defun counsel-bm (&optional initial-input)
  "Use ivy to select bm bookmarks.
It has the ability to preview the bookmarks like `swiper-all'."
  (interactive)
  (let* ((bm-list (bm-counsel-get-list (bm-overlays-lifo-order t)))
         (bm-hash-table (make-hash-table :test 'equal))
         (search-list (-map (lambda (bm) (car bm)) bm-list)))

    (-each bm-list (lambda (bm)
                     (puthash (car bm) (cdr bm) bm-hash-table)))

    (if search-list
        (ivy-read "Find bookmark: "
                  search-list
                  :keymap counsel-describe-map

                  :action (lambda (chosen)
                            (let ((bookmark (gethash chosen bm-hash-table)))
                              (switch-to-buffer (overlay-buffer bookmark))
                              (bm-goto bookmark)))

                  :update-fn #'counsel-bm-update-input

                  :initial-input initial-input
                  :caller 'counsel-bm
                  )
      (message "%s" "No bookmark now."))))

  (defun counsel-bm-from-isearch ()
  "Invoke `counsel-bmr' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (counsel-bm query)))
  
  (define-key isearch-mode-map (kbd "<M-return>") 'counsel-bm-from-isearch)
  )

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1)
  (when (icons-displayable-p)
    (with-no-warnings
      ;; Display icons for bookmarks
      (defun my-bookmark-bmenu--revert ()
        "Re-populate `tabulated-list-entries'."
        (let (entries)
          (dolist (full-record (bookmark-maybe-sort-alist))
            (let* ((name       (bookmark-name-from-full-record full-record))
                   (annotation (bookmark-get-annotation full-record))
                   (location   (bookmark-location full-record))
                   (file       (file-name-nondirectory location))
                   (icon       (cond
                                ((file-remote-p location)
                                 (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.0))
                                ((file-directory-p location)
                                 (all-the-icons-icon-for-dir location :height 0.9 :v-adjust 0.01))
                                ((not (string-empty-p file))
                                 (all-the-icons-icon-for-file file :height 0.9 :v-adjust 0.0)))))
              (push (list
                     full-record
                     `[,(if (and annotation (not (string-equal annotation "")))
                            "*" "")
                       ,icon
                       ,(if (display-mouse-p)
                            (propertize name
                                        'font-lock-face 'bookmark-menu-bookmark
                                        'mouse-face 'highlight
                                        'follow-link t
                                        'help-echo "mouse-2: go to this bookmark in other window")
                          name)
                       ,@(if bookmark-bmenu-toggle-filenames
                             (list location))])
                    entries)))
          (tabulated-list-init-header)
          (setq tabulated-list-entries entries))
        (tabulated-list-print t))
      (advice-add #'bookmark-bmenu--revert :override #'my-bookmark-bmenu--revert)

      (defun my-bookmark-bmenu-list ()
        "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
        (interactive)
        (bookmark-maybe-load-default-file)
        (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
          (if (called-interactively-p 'interactive)
              (pop-to-buffer buf)
            (set-buffer buf)))
        (bookmark-bmenu-mode)
        (bookmark-bmenu--revert))
      (advice-add #'bookmark-bmenu-list :override #'my-bookmark-bmenu-list)

      (define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
        (setq truncate-lines t)
        (setq buffer-read-only t)
        (setq tabulated-list-format
              `[("" 1) ;; Space to add "*" for bookmark with annotation
                ("" 2) ;; Icons
                ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
                ,@(if bookmark-bmenu-toggle-filenames
                      '(("File" 0 bookmark-bmenu--file-predicate)))])
        (setq tabulated-list-padding bookmark-bmenu-marks-width)
        (setq tabulated-list-sort-key '("Bookmark" . nil))
        (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
        (setq revert-buffer-function #'bookmark-bmenu--revert)
        (tabulated-list-init-header)))))

(provide 'init-bookmark)
