(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; net abbrev

    ;; english word abbrev

    ;; programing
    ("utf8" "-*- coding: utf-8 -*-" )

    ("hr" "--------------------------------------------------" )
    ("bu" "•" )
    ("catface" "😸" )
    ("pigface" "🐷")
    ("dogface" "🐶")
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜" )
    ("ra" "→" )
    ;; quick file path
    ("lorg" "/mnt/c/Users/lixun/Documents/org/")
    ("lpw" "/mnt/c/Users/lixun/Documents/python_work/")
    ("ldl" "/mnt/c/Users/lixun/Downloads/")
    ("led" "~/.emacs.default/")
    ))

(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (abbrev-mode 1)
   (setq local-abbrev-table global-abbrev-table)))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(provide 'init-abbrev)
