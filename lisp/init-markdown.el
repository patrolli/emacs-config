(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t

        markdown-content-type "application/xhtml+xml"
        markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre code').forEach((code) => {
    if (code.className != 'mermaid') {
      hljs.highlightBlock(code);
    }
  });
});
</script>
<script src='https://unpkg.com/mermaid@8.4.8/dist/mermaid.min.js'></script>
<script>
mermaid.initialize({
  theme: 'default',  // default, forest, dark, neutral
  startOnLoad: true
});
</script>
"
        markdown-gfm-additional-languages "Mermaid")

  ;; `multimarkdown' is necessary for `highlight.js' and `mermaid.js'
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))

  ;; Use `which-key' instead
  (with-no-warnings
    (advice-add #'markdown--command-map-prompt :override #'ignore)
    (advice-add #'markdown--style-map-prompt   :override #'ignore))
  :config
  (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode))

  ;; Table of contents
  (use-package markdown-toc
    :ensure t
    :bind (:map markdown-mode-command-map
           ("r" . markdown-toc-generate-or-refresh-toc))))

;; rich-style will affect the style of either the selected region,
;; or the current line if no region is selected.
;; style may be an atom indicating a rich-style face,
;; e.g. 'italic or 'bold, using
;;   (put-text-property START END PROPERTY VALUE &optional OBJECT)
;; or a color string, e.g. "red", using
;;   (facemenu-set-foreground COLOR &optional START END)
;; or nil, in which case style will be removed.
(defun rich-style (style)
  (let* ((start (if (use-region-p)
                    (region-beginning) (line-beginning-position)))
                    
         (end   (if (use-region-p)
                    (region-end)  (line-end-position))))
    (cond
     ((null style)      (set-text-properties start end nil))
     ((stringp style)   (facemenu-set-foreground style start end))
     (t                 (add-text-properties start end (list 'face style)))
     )))

(defun rich-style-add-front-color()
  (interactive)
  (let ((color (completing-read "Choose color" '("red" "blue" "sea green"))))
  (rich-style color)))

(defvar rich-style-bg-color-history nil)
(defun rich-style-add-bg-color ()
  (interactive)
  (let* (
	 (color (completing-read "Choose color: " '("red" "blue" "green") nil t nil 'rich-style-bg-color-history))
	 (start (if (use-region-p)
                    (region-beginning) (line-beginning-position)))
         (end   (if (use-region-p)
                    (region-end)  (line-end-position))))
    (setq rich-style-bg-color-history (cons color rich-style-bg-color-history))
    (facemenu-set-background color start end)
    (facemenu-set-foreground "white" start end)))

;; (add-hook 'text-mode-hook 'enriched-mode)
;; (add-hook 'text-mode-hook #'(lambda () (enriched-mode -1)))
;; (add-hook 'git-commit-setup-hook #'(lambda () (enriched-mode -1)))
;; (add-hook 'prog-mode-hook #'(lambda () (enriched-mode -1)))
(use-package enriched
  :config
    (add-hook 'enriched-mode-hook #'(lambda () (keymap-set enriched-mode-map "<remap> <newline-and-indent>" 'nil)))
    (add-hook 'git-commit-setup-hook #'(lambda () (enriched-mode -1)))
    (add-hook 'prog-mode-hook #'(lambda () (enriched-mode -1)))
    (add-hook 'markdown-mode-hook #'(lambda () (enriched-mode -1)))
    (define-key enriched-mode-map (kbd "RET") #'newline-and-indent))

(defhydra dh-hydra-markdown-mode (:hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item   

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
 
"


  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim) 
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)  

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue) 
)


(global-set-key [f9] 'dh-hydra-markdown-mode/body)

;; Add an inline HTML tag:
(defun add-html-tag (tag) (interactive "sHTML Tag")
  (let (
        (rstart (if (region-active-p) (region-beginning) (point)))
        (rend   (if (region-active-p) (region-end)       (point))))

    ;; Insert the close tag first, because inserting the open tag
    ;; will mess up the rend position.
    (goto-char rend)
    (insert "</" tag ">")

    ;; Now the open tag:
    (goto-char rstart)
    (insert "<" tag ">")
))

(defun add-html-font-color (color)
  (interactive "sFont color ")
  (let (
        (rstart (if (region-active-p) (region-beginning) (point)))
        (rend   (if (region-active-p) (region-end)       (point))))

    (goto-char rend)
    (insert "</font>")

    (goto-char rstart)
    (insert "<font color=\'" color "\'>")
))

(defvar md-todo-states-color-alist
  '(("TODO" . "Chartreuse")
    ("DOING" . "Orange")
    ("DONE" . "DodgerBlue"))
  "An alist mapping TODO states to their corresponding colors.")

(defun md-insert-task-tag ()
  "Insert a task tag in a Markdown file with distinct colors."
  (interactive)
  (let* ((tag (completing-read "Select task tag (TODO/DOING/DONE): " (mapcar 'car md-todo-states-color-alist) nil t))
         (color (cdr (assoc tag md-todo-states-color-alist))))
    (insert (format "<b><font color='%s'>%s</font></b>" color tag))))

(defun md-toggle-todo-state ()
  "Toggle TODO state in the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward-regexp "<b><font color='\\([A-Za-z]+\\)'>\\(TODO\\|DOING\\|DONE\\)</font></b>" (line-end-position) t)
      (let* ((color (match-string 1))
             (todo-state (match-string 2))
             (next-state (cond
                           ((equal todo-state "TODO") "DOING")
                           ((equal todo-state "DOING") "DONE")
                           ((equal todo-state "DONE") "TODO")
                           (t ""))) ; fallback to empty string
             (next-color (cdr (assoc next-state md-todo-states-color-alist))))
        (replace-match (format "<b><font color='%s'>%s</font></b>" next-color next-state))))))


(provide 'init-markdown)
