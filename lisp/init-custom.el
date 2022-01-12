(require 'init-const)

(defgroup lxs nil
  "Lixunsong's Emacs customization."
  :group 'convenience)

(defcustom lxs-home-dir (if sys/wslp "/mnt/c/Users/lixun/" "~/")
  "choose home dir for wsl"
  :group 'lxs
  :type 'string)

(defcustom lxs-full-name "patrolli"
  "user full name"
  :group 'lxs
  :type 'string)

(defcustom lxs-mail-address "lixunsonghcl@126.com"
  "user email address"
  :group 'lxs
  :type 'string)

(defcustom lxs-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'lxs
  :type 'boolean)

(defcustom lxs-proxy "127.0.0.1:20171"
  "Set network proxy"
  :group 'lxs
  :type 'string)

(defcustom lxs-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :group 'lxs
  :type 'string)

(defcustom lxs-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("map" . ?↦)
    ("/=" . ?≠)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("=<<" . (?= (Br . Bl) ?≪))
    (">>=" . (?≫ (Br . Bl) ?=))
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("not" . ?¬))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'lxs
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom lxs-prettify-org-symbols-alist
  '(
    ("#+ARCHIVE:" . ?📦)
    ("#+AUTHOR:" . ?👤)
    ("#+CREATOR:" . ?💁)
    ("#+DATE:" . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:" . ?📧)
    ("#+OPTIONS:" . ?⛭)
    ("#+SETUPFILE:" . ?⛮)
    ("#+TAGS:" . ?🏷)
    ("#+TITLE:" . ?📓)
    ("#+BEGIN_SRC" . ?➤)
    ("#+begin_src" . ?➤)
    ("#+END_SRC" . ?➤)
    ("#+end_src" . ?➤)
    ("#+begin_quote" . ?«)
    ("#+end_quote" . ?«)
    ("#+DOWNLOADED:" . ?)
    ("#+HEADERS" . ?☰)
    ("#+RESULTS:" . ?💻))
  "Alist of symbol prettifications for `org-mode'."
  :group 'lxs
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ;; NOTE: 这里只是设置 custom file, 加载放在 init-package.el 中

(provide 'init-custom)
