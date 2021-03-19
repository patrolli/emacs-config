
(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and
   (display-graphic-p)
       (require 'all-the-icons nil t)))

(provide 'init-funcs)
