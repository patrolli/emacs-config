
(use-package bm
  :ensure t
  :defer t
  :demand t
  :init
  (setq bm-restore-repository-on-load t)
  :hook
  ((after-save-hook . bm-buffer-save)
   (kill-buffer-hook . bm-buffer-save)
   (kill-emacs-hook . (lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))
   (find-file-hooks . bm-buffer-restore)
   (after-revert-hook . bm-buffer-restore)
   (vc-before-checkin-hook bm-buffer-save))
  :config
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file (concat user-emacs-directory "bm-repository"))
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  
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

(provide 'init-bookmark)






