(require 'package)

(setq package-enable-at-startup t)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/"   )
        ("org"   . "http://orgmode.org/elpa/"     )))
(message "Initializing packages")
(package-initialize)
(message "done")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package htmlize
  :ensure t)

(use-package raku-mode
  :ensure t)

(require 'org)

(use-package org-plus-contrib
  :ensure t)

(use-package org-src
  :ensure t)

(use-package org-src
  :config
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t))

(load-theme 'tsdh-light)

(defun cc4s/publish-site ()
  (interactive)
  (require 'cl-macs)
  (cl-macrolet ((cc4s/log (fmt &rest args)
                          `(message (format ,(concat "\x1b[35m∀ "
                                                     fmt
                                                     "\x1b[0m")
                                            ,@args))))
    (let* ((site-file-path (file-name-directory (buffer-file-name)))
           (publish-directory (format "%s/build/" site-file-path))
           (org-publish-project-alist
            `(("site"
               :base-directory ,site-file-path
               :publishing-directory ,publish-directory
               :section-numbers nil
               :table-of-contents t
               :publishing-function org-html-publish-to-html
               ;;:publishing-function org-html-export-to-html
               :htmlized-source nil
               :language en
               :exclude ".*templates.*"
               :recursive t))))
      (cc4s/log "Publishing site")
      (cc4s/log "build directory %s" publish-directory)
      (org-publish-current-project))))
