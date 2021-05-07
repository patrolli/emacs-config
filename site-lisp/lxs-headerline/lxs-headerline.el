
(defgroup lxs-headerline nil
  "lxs's headerline"
  :prefix "lxs-headerline-"
  :group 'lxs-headerline)

(defcustom lxs-headerline-breadcrumb-segments '(file show-function)
  "Face used on breadcrumb text on modeline."
  :type '(repeat
          (choice (const :tag "Include the project name." project)
                  (const :tag "Include the open file name." file)
                  (const :tag "Include the directories up to project." path-up-to-project)
		  (const :tag "Show function in current cursor." show-function)
                  ))
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-separator-face '((t :inherit shadow :height 0.8))
  "Face used for breadcrumb separator on headerline."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-path-face '((t :inherit font-lock-string-face))
  "Face used for breadcrumb paths on headerline."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-path-error-face
  '((t :underline (:style wave :color "Red1")
       :inherit lxs-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an error under that path"
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-path-warning-face
  '((t :underline (:style wave :color "Yellow")
       :inherit lxs-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an warning under that path"
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-path-info-face
  '((t :underline (:style wave :color "Green")
       :inherit lxs-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an info under that path"
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-path-hint-face
  '((t :underline (:style wave :color "Green")
       :inherit lxs-headerline-breadcrumb-path-face))
  "Face used for breadcrumb paths on headerline when there is an hint under that path"
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-project-prefix-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face used for breadcrumb prefix on headerline.
Only if `lxs-headerline-breadcrumb-prefix` is `project-name-only`."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-unknown-project-prefix-face
  '((t :inherit shadow :weight bold))
  "Face used for breadcrumb prefix on headerline.
Only if `lxs-headerline-breadcrumb-prefix` is `project-name-only`."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-symbols-face
  '((t :inherit font-lock-doc-face :weight bold))
  "Face used for breadcrumb symbols text on headerline."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-symbols-error-face
  '((t :inherit lxs-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Red1")))
  "Face used for breadcrumb symbols text on headerline when there
is an error in symbols range."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-symbols-warning-face
  '((t :inherit lxs-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Yellow")))
  "Face used for breadcrumb symbols text on headerline when there
is an warning in symbols range."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-symbols-info-face
  '((t :inherit lxs-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Green")))
  "Face used for breadcrumb symbols text on headerline when there
is an info in symbols range."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-symbols-hint-face
  '((t :inherit lxs-headerline-breadcrumb-symbols-face
       :underline (:style wave :color "Green")))
  "Face used for breadcrumb symbols text on headerline when there
is an hints in symbols range."
  :group 'lxs-headerline)

(defface lxs-headerline-breadcrumb-deprecated-face
  '((t :inherit lxs-headerline-breadcrumb-symbols-face
       :strike-through t))
  "Face used on breadcrumb deprecated text on modeline."
  :group 'lxs-headerline)

(defvar-local lxs-headerline--string nil
  "Holds the current breadcrumb string on headerline.")

(defvar lxs-headerline-arrow nil
  "Holds the current breadcrumb string on headerline.")

(defvar-local lxs-headerline--path-up-to-project-segments nil
  "Holds the current breadcrumb path-up-to-project segments for
caching purposes.")

(defun lxs-icons-all-the-icons-material-icon (icon-name face fallback)
  "Get a material icon from all-the-icons by ICON-NAME using FACE.
Fallback to FALLBACK string if not found or not available."
  (if (functionp 'all-the-icons-material)
      (all-the-icons-material icon-name
                              :face face)
    (propertize fallback 'face face)))

(defun lxs-headerline--arrow-icon ()
  "Build the arrow icon for headerline breadcrumb."
  (or
   lxs-headerline-arrow
   (setq lxs-headerline-arrow (lxs-icons-all-the-icons-material-icon
                               "chevron_right"
                               'lxs-headerline-breadcrumb-separator-face
                               ">"
                               ))))


(defun lxs-headerline--with-action (display-string)
  "Assign LOCAL-MAP and HELP-ECHO-STRING to the region around the DISPLAY-STRING."
  (propertize display-string
              'mouse-face 'header-line-highlight
              ))


(defun lxs-headerline--directory-with-action (directory-display-string)
  "Build action for FULL-PATH and DIRECTORY-DISPLAY-STRING."
  (lxs-headerline--with-action directory-display-string))

(defun lxs-headerline--path-up-to-project-root (root-path path)
  "Find recursively the folders until the project ROOT-PATH.
PATH is the current folder to be checked."
  (let ((current-path path)
        headerline-path-components)
    (while (not (f-same? root-path current-path))
      (push (lxs-headerline--directory-with-action 
                                                   (f-filename current-path))
            headerline-path-components)
      (setq current-path (f-parent current-path)))
    headerline-path-components))

(defun lxs-headerline--build-project-string ()
  "Build the project-segment string for the breadcrumb."
  (-if-let (root (projectile-project-root))
      (propertize (lxs-headerline--directory-with-action
                   (f-filename root))
                  'font-lock-face
                  'lxs-headerline-breadcrumb-project-prefix-face)
    (propertize "<unknown>"
                'font-lock-face
                'lxs-headerline-breadcrumb-unknown-project-prefix-face)))

(defun lxs-headerline--build-file-string ()
  "Build the file-segment string for the breadcrumb."
  (let* ((file-path (buffer-file-name))
         (filename (f-filename file-path)))
    (if-let ((file-ext (f-ext file-path)))
        (concat (all-the-icons-icon-for-buffer)
                " "
                (propertize filename
                            'font-lock-face
                            'lxs-headerline-breadcrumb-project-prefix-face))
      filename)))

(defun lxs-headerline--build-function-string ()
  (if (which-function)
        (propertize (which-function)
	      'font-lock-face
              'lxs-headerline-breadcrumb-project-prefix-face
	      )
    (propertize "")
      )
)


(defun lxs-headerline--build-path-up-to-project-string ()
  "Build the path-up-to-project segment for the breadcrumb."
  (if-let ((root (projectile-project-root)))
      (let ((segments (or
                       lxs-headerline--path-up-to-project-segments
                       (setq lxs-headerline--path-up-to-project-segments
                             (lxs-headerline--path-up-to-project-root
                              root
                              (f-parent (buffer-file-name)))))))
        (mapconcat (lambda (next-dir)
                     (propertize next-dir
                                 'font-lock-face
                                 'lsp-headerline-breadcrumb-path-face))
                   segments
                   (concat " " (lxs-headerline--arrow-icon) " ")))
    ""))

(defun lxs-headerline--build-string ()
  "Build the header-line string."
  (string-trim-right
   (mapconcat
    (lambda (segment)
      (let ((segment-string
             (pcase segment
               ('project (lxs-headerline--build-project-string))
               ('file (lxs-headerline--build-file-string))
               ('path-up-to-project (lxs-headerline--build-path-up-to-project-string))
	       ('show-function (lxs-headerline--build-function-string))
               )))
        (if (eq segment-string "")
            ""
          (concat (lxs-headerline--arrow-icon)
                  " "
                  segment-string
                  " "))))
    lxs-headerline-breadcrumb-segments
    "")))


;; (setq lxs-headerline--string (lxs-headerline--build-string))
;; (defun lxs-headerline-set ()
;;   (setq header-line-format '(t (:eval (lxs-headerline--build-string))))
;;   )
;; (lxs-headerline-set)
(add-hook 'buffer-list-update-hook
          'lxs-headerline-set)

(remove-hook 'buffer-list-update-hook 'lxs-headerline-set)
(setq header-line-format nil)
;; (setq header-line-format
;;               '((which-func-mode ("" which-func-format " "))))





