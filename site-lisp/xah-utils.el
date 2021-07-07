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


(defun xah-show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy history*.
URL `http://ergoemacs.org/emacs/emacs_show_kill_ring.html'
Version 2019-12-02"
  (interactive)
  (let (($buf (generate-new-buffer "*copy history*")))
    (progn
      (switch-to-buffer $buf)
      (funcall 'fundamental-mode)
      (dolist (x kill-ring )
        (insert x "\n\nhh=============================================================================\n\n"))
      (goto-char (point-min)))))

(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2019-12-26 2021-04-04"
  (interactive)
  (if (use-region-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

(defvar bookmark-alist)
(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(defun xah-open-file-fast ()
  "Prompt to open a file from bookmark `bookmark-bmenu-list'.
This command is similar to `bookmark-jump', but use `ido-mode' interface, and ignore cursor position in bookmark.
URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2019-02-26"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let (($this-bookmark
         (ivy-completing-read "Open bookmark:" (mapcar (lambda ($x) (car $x)) bookmark-alist))))
    (find-file (bookmark-get-filename $this-bookmark))
    ;; (bookmark-jump $this-bookmark)
    ))

(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.
URL `http://ergoemacs.org/emacs/emacs_form_feed_section_paging.html'
Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

;; [[file:/mnt/c/Users/lixun/Documents/org/static/img/capture_2021_05_10_11_59_03.png]]


(setq initial-major-mode (quote text-mode))
(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
It returns the buffer (for elisp programing).
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))
(setq initial-buffer-choice 'xah-new-empty-buffer)

(defvar lxs-major-mode-to-file-postfix '((emacs-lisp-mode . ".el") (python-mode . ".py") (org-mode . ".org") (lisp-interaction-mode . ".el"))
  "map major mode to file postfix string"
  )

(defvar lxs-fast-save-dir "~/Documents/"
  "defalut directory for save untitled buffer temporary")

(defun lxs/fast-save-untitled-buffer ()
  "Fast save untitled buffer into a pre-defined directory.
File name is determined by the date and buffer major mode.
When the file save name is duplicated, add a number at the
end make each untitled buffer saved uniquely."
  (interactive)
  (let* ((untitled-dir "~/Documents/")
	 (save-name (concat (format-time-string "%Y-%m-%d") "-" (buffer-name)))
	 (postfix (cdr (assoc major-mode lxs-major-mode-to-file-postfix))))  
    (let ((target (concat untitled-dir save-name postfix))
	  (num 1))
      (while (f-exists-p target)
	(setq target (concat untitled-dir save-name "-" (number-to-string num) postfix))
	(setq num (1+ num)))
      (write-file target)
      )
))

(defvar xah-recently-closed-buffers nil "alist of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")

(defcustom xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'."
  :type 'integer
  :group 'xah-fly-keys)

(declare-function minibuffer-keyboard-quit "delsel" ())
(declare-function org-edit-src-save "org-src" ())
(defun xah-close-current-buffer ()
  "Close the current buffer.
Similar to `kill-buffer', with the following addition:
• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2018-06-11"
  (interactive)
  (let (($org-p (string-match "^*Org Src" (buffer-name))))
    (if (string-equal major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (xah-user-buffer-q)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   $org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))
        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))
        ;; (kill-buffer-and-window )
	(kill-buffer)
	))))

(defun xah-open-last-closed ()
  "Open the last closed file.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (if (> (length xah-recently-closed-buffers) 0)
      (find-file (cdr (pop xah-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun xah-open-recently-closed ()
  "Open recently closed file.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (find-file (ivy-completing-read "open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (let (($buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer $buf)
    (mapc (lambda ($f) (insert (cdr $f) "\n"))
          xah-recently-closed-buffers)))


(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

(defun xah-select-current-block ()
  "Select the current block of text between blank lines.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-07-02"
  (interactive)
  (progn
    (skip-chars-forward " \n\t")
    (when (re-search-backward "\n[ \t]*\n" nil "move")
      (re-search-forward "\n[ \t]*\n"))
    (push-mark (point) t t)
    (re-search-forward "\n[ \t]*\n" nil "move")))

(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2019-12-26 2021-04-04"
  (interactive)
  (if (use-region-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))



(defvar xah-run-current-file-before-hook nil "Hook for `xah-run-current-file'. Before the file is run.")

(defvar xah-run-current-file-after-hook nil "Hook for `xah-run-current-file'. After the file is run.")

(defvar xah-run-current-file-map nil "A association list that maps file extension to program path, used by `xah-run-current-file'. First element is file suffix, second is program name or path. You can add items to it.")
(setq
 xah-run-current-file-map
 '(
   ("pl" . "perl")
   ("py" . "python")
   ("py3" . "python3")
   ("rb" . "ruby")
   ("sh" . "bash")
   ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
   ("tex" . "pdflatex")
   ("latex" . "pdflatex")
   ))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.
File suffix is used to determine which program to run, set in the variable `xah-run-current-file-map'.
If the file is modified or not saved, save it automatically before run.
URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2020-09-24 2021-01-21"
  (interactive)
  (let (
        ($outBuffer "*xah-run output*")
        (resize-mini-windows nil)
        ($suffixMap xah-run-current-file-map )
        $fname
        $fSuffix
        $progName
        $cmdStr)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fSuffix (file-name-extension $fname))
    (setq $progName (cdr (assoc $fSuffix $suffixMap)))
    (setq $cmdStr (concat $progName " "   $fname  " &"))
    (run-hooks 'xah-run-current-file-before-hook)
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     (t (if $progName
            (progn
              (message "Running")
              (shell-command $cmdStr $outBuffer )
	      )
          (error "No recognized program file suffix for this file."))))
    (run-hooks 'xah-run-current-file-after-hook)))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01 2021-03-19"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let (($p1 (point)))
                (end-of-visual-line 1)
                (when (eq $p1 (point))
                  (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (set-mark (point))
               (end-of-visual-line))
      (progn
        (end-of-line)
        (set-mark (line-beginning-position))))))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.
when there's no selection,
• if cursor is on a any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.
when there's a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2020-02-04"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        ;; (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-syntax-forward "_w")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        ;; (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next text block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))
       ;;
       ))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, does not consider nesting. For example, if text is
 (a(b)c▮)
 the selected char is “c”, not “a(b)c”.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2020-11-24"
  (interactive)
  (let (
        ($skipChars "^\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
        $p1
        )
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22 2020-09-08"
  (interactive)
  (let ($begin $end)
    (if (use-region-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      (message "white space cleaned"))))

(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22 2020-09-08"
  (interactive)
  (let ($begin $end)
    (if (use-region-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))

(provide 'xah-utils)
