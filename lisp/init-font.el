(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
  ;; Specify font for all unicode characters
(cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
         when (font-installed-p font)
         return(set-fontset-font t 'unicode font nil 'prepend))

(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("文泉驿等宽正黑-12.0" "SF Mono" "Hack" "Source Code Pro" "Fira Code"
                           "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :font font
                                        :height (cond (sys/macp 130)
                                                      (sys/win32p 120)
                                                      (t 100))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    ;; (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
    ;;          when (font-installed-p font)
    ;;          return (set-fontset-font t 'emoji `(,font . "iso10646-1") nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff) font))))

(centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

(provide 'init-font)
