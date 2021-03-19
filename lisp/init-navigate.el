(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
(defvar navigate-bar--title (with-faicon "bicycle" "Navigate Bar" 1 -0.05))
(pretty-hydra-define hydra-navigate-bar (:foreign-keys warn :title navigate-bar--title :quit-key "q")
  ("Basic"
   (("k" previous-line "")
    ("j" next-line "")
    ("h" backward-char "")
    ("l" forward-char "")
    ("<" beginning-of-buffer)
    (">" end-of-buffer))
   "Bookmark"
   (("b n" bm-next "next bookmark")
    ("b p" bm-previous "previous bookmark")
    ("b s" bm-toggle "toggle bookmark")
    ("b l" counsel-bm "show bookmarks"))
    "Mark"
    (("p" xah-pop-local-mark-ring "local mark")
     ("P" pop-global-mark "all buffer mark")
     ("m l" counsel-mark-ring "show local marks"))
    "hl-todo"
    (("f n" hl-todo-nexe "next TODO")
     ("f p" hl-todo-previous "previous TODO")
     ("f o" hl-todo-occur "show all TODOs")
     ("f i" hl-todo-insert "insert TODO")
     )))
(global-set-key (kbd "C-x n") 'hydra-navigate-bar/body)
(provide 'init-navigate)
