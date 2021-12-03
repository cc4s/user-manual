(defconst *cc4s-start-time* (current-time))
(defmacro cc4s-log (fmt &rest args)
  `(let ((elapsed (time-to-seconds (time-since *cc4s-start-time*))))
     (message ,(concat "%d "
                       "\x1b[35m∷ "
                       fmt
                       "\x1b[0m")
              elapsed ,@args)))
(defun !!done () (cc4s-log "\t✓ done"))

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(cc4s-log "Requiring package")
(require 'package)

(setq package-enable-at-startup t)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/"   )
        ("org"   . "http://orgmode.org/elpa/"     )))

(cc4s-log "Initializing packages")
(package-initialize)



(cc4s-log "Setting up use-package")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(cc4s-log "Requiring use-package")
(eval-when-compile
  (require 'use-package))

(cc4s-log "htmlize")
(use-package htmlize
  :defer t
  :ensure t)

(cc4s-log "loading org-ref and citeproc")
(use-package citeproc :defer t :ensure t)
(use-package org-ref :defer t :ensure t)

(cc4s-log "raku-mode")
(use-package raku-mode
  :defer t
  :ensure t)

(cc4s-log "requiring org")
(require 'org)

(cc4s-log "up org-contrib")
(use-package org-plus-contrib
  :defer t
  :ensure t
  :config
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t))

(cc4s-log "loading theme..")
(load-theme 'tsdh-light)
(!!done)

(defvar cc4s/html-head-libs
  "
    <meta charset='UTF-8'>
    <meta name='apple-mobile-web-app-capable' content='yes'>
    <meta name='viewport' content='width=device-width, initial-scale=1'>
    <link
      href='https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css'
      rel='stylesheet'
      integrity='sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU'
      crossorigin='anonymous'>
    <script
      src='https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/js/bootstrap.bundle.min.js'
      integrity='sha384-/bQdsTh/da6pkI1MST/rWKFNjaCP5gBSY4sEBT38Q/9RBh9AH40zEOg7Hlq2THRZ'
      crossorigin='anonymous'></script>
    <style>
       #content {
          padding: 5%;
       }
    </style>
  ")

(defvar cc4s/root "/user-manual/")

(defvar cc4s/navigation-bar
  (format "
  <nav class='navbar navbar-expand-lg navbar-light bg-light'>
    <div class='container-fluid'>
      <a class='navbar-brand' href='#'>
        Cc4s
      </a>
      <button class='navbar-toggler' type='button'
              data-bs-toggle='collapse'
              data-bs-target='#navbarSupportedContent'
              aria-controls='navbarSupportedContent'
              aria-expanded='false'
              aria-label='Toggle navigation'>
        <span class='navbar-toggler-icon'></span>
      </button>
      <div class='collapse navbar-collapse' id='navbarSupportedContent'>
        <ul class='navbar-nav me-auto mb-2 mb-lg-0'>
          <li class='nav-item'>
            <a class='nav-link' aria-current='page' href='%sindex.html'>
              <i class='fa fa-home'></i>
              Home
            </a>
          </li>
          <li class='nav-item'>
            <a class='nav-link' href='%1$ssitemap.html'>
              <i class='fa fa-sign-out'></i>
              Sitemap
            </a>
          </li>
        </ul>
      </div>
    </div>
  </nav>
 " cc4s/root))

(defun cc4s/publish-to-html (plist filename pub-dir)
  (require 'org-ref)
  (require 'org-ref-refproc)
  (let ((org-export-before-parsing-hook '(;org-ref-cite-natmove
                                          org-ref-csl-preprocess-buffer
                                          org-ref-refproc)))
    (org-html-publish-to-html plist filename pub-dir)))

(defun cc4s/publish-site ()
  (interactive)
  (require 'cl-macs)
  (let* ((site-file-path (file-name-directory (buffer-file-name)))
         (publish-directory (format "%s/user-manual/" site-file-path))
         (org-publish-timestamp-directory
          (format "%s/.emacs/org-timestamps" site-file-path))
         (org-publish-project-alist
          `(("data"
             :base-directory ,(format "%s/data" site-file-path)
             :publishing-directory ,(format "%s/data" publish-directory)
             :publishing-function org-publish-attachment
             :base-extension ".*"
             :recursive t)
            ("site"
             :base-directory ,site-file-path
             :base-extension "org"
             :publishing-directory ,publish-directory
             :with-creator nil
             :with-author nil
             :section-numbers t
             :table-of-contents t
                                        ;:publishing-function org-html-publish-to-html
             :publishing-function cc4s/publish-to-html
             ;;:publishing-function org-html-export-to-html
             :htmlized-source nil
             :html-validation-link nil
             :html-head-extra ,cc4s/html-head-libs
             :language en
                                        ;:html-use-infojs nil
                                        ;:html-link-home "sitemap.html"
             :auto-sitemap t
             :html-preamble ,cc4s/navigation-bar
             :html-self-link-headlines t
             :sitemap-title "Sitemap"
             :exclude "config/*"
             :recursive t))))
    (cc4s-log "Publishing cc4s user manual")
    (cc4s-log "build directory %s" publish-directory)
    (org-publish-all)
    (!!done)))