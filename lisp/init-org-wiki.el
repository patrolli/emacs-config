(require 'ox-publish)
(require 'org-roam-export)


(defun xs-org-publish-org-sitemap (title list)
  "Sitemap generation function."
  (concat "#+TITLE: Sitemap\n\n"
          (org-list-to-subtree list)
	  ))

(defun xs-org-publish-org-sitemap-format (entry style project)
  "Custom sitemap entry formatting: add date"
  (cond ((not (directory-name-p entry))
         (format "[[file:%s][(%s) %s]]"
                 entry
                 (format-time-string "%Y-%m-%d"
                                     (org-publish-find-date entry project))
                 (org-publish-find-title entry project)))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))


(setq org-publish-project-alist
      '(
       ("org-notes"
	:base-directory "~/Documents/org/org-roam-files/"
	:base-extension "org"
	:publishing-directory "~/Documents/org/publish_html/"
	:recursive t
	:publishing-function org-html-publish-to-html
	:headline-levels 4             ; Just the default for this project.
	:auto-preamble t
	:auto-sitemap t
	:sitemap-filename  "sitemap.org"   ; ... 称它为 sitemap.org（它是默认的）... 
	:sitemap-title  "Sitemap"          ; ...标题为“站点地图”。
	:sitemap-function xs-org-publish-org-sitemap
	:sitemap-format-entry xs-org-publish-org-sitemap-format
	:with-toc t
	)
       ("org-static"
	:base-directory "~/Documents/org/static/"
	:base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	:publishing-directory "~/public_html/"
	:recursive t
	:publishing-function org-publish-attachment
	)
       ("org" :components ("org-notes" "org-static"))))

(setq org-export-use-babel nil)
(setq org-export-with-broken-links 'mark)
(setq org-html-htmlize-output-type 'css)
(setq org-html-head-include-default-style nil)


(provide 'init-org-wiki)
