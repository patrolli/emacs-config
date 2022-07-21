(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; ✅☑️✔️❌❎
    
    ;; net abbrev

    ;; english word abbrev

    ;; programing
    ("utf8" "-*- coding: utf-8 -*-" )

    ("hr" "--------------------------------------------------" )
    ;; ("bu" "•" )
    ("catface" "😸" )
    ("pigface" "🐷")
    ("dogface" "🐶")
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜" )
    ("ra" "→" )
    ("chm" "✅")
    ("crm" "❌")
    ;; quick file path
    ("lorg" "/mnt/c/Users/lixun/Documents/org/")
    ("lpw" "/mnt/c/Users/lixun/Documents/python_work/")
    ("ldl" "/mnt/c/Users/lixun/Downloads/")
    ("led" "~/.emacs.default/")
    ("lcb" "[ ]")
    ("limg" "#+ATTR_HTML: :width 800px\n#+ATTR_ORG: :width 800px")
    ))

(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (abbrev-mode 1)
   (setq local-abbrev-table global-abbrev-table)))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(provide 'init-abbrev)
