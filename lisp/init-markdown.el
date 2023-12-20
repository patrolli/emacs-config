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
  (let ((color (completing-read "Choose color" '("red" "blue" "sea green" "purple"))))
  (rich-style color)))

(defvar rich-style-bg-color-history nil)
(defun rich-style-add-bg-color ()
  (interactive)
  (let* (
	 (color (completing-read "Choose color: " '("red" "blue" "green" "purple" "grey") nil t nil 'rich-style-bg-color-history))
	 (start (if (use-region-p)
                    (region-beginning) (line-beginning-position)))
         (end   (if (use-region-p)
                    (region-end)  (line-end-position))))
    (setq rich-style-bg-color-history (cons color rich-style-bg-color-history))
    (facemenu-set-background color start end)
    (facemenu-set-foreground "white" start end)))

(use-package enriched
  :config
  (keymap-set enriched-mode-map "<remap> <newline-and-indent>" nil)
  (define-key enriched-mode-map (kbd "<return>") #'newline-and-indent)
  ;; (keymap-set enriched-mode-map "<remap> <reindent-then-newline-and-indent>" nil)
  )

;; (add-hook 'text-mode-hook 'enriched-mode)
;; (add-hook 'git-commit-setup-hook #'(lambda () (enriched-mode -1)))
;; (add-hook 'prog-mode-hook #'(lambda () (enriched-mode -1)))



(provide 'init-markdown)
