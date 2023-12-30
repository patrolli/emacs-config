;; advanced systax highting
(use-package tree-sitter
  :config
  :hook
  ((python-mode c++-mode c-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(provide 'init-highlight)
