(setq package-enable-at-startup nil)
(require 'subr-x)
(defconst +cc4s-start-time+ (current-time))
(defmacro cc4s-log (fmt &rest args)
  `(let ((elapsed (time-to-seconds (time-since +cc4s-start-time+))))
     (message ,(concat "%d "
                       "\x1b[35m∷CC4S» "
                       fmt
                       "\x1b[0m")
              elapsed ,@args)))
(defun !!done () (cc4s-log "\t✓ done"))
(defmacro use-package! (name &rest body)
  `(progn
     (cc4s-log "\t→ %s" ',name)
     (use-package ,name ,@body)))

(defvar cc4s-root-directory (expand-file-name
                             (format "%s../"
                                     (file-name-directory load-file-name))))

(setq package-user-dir (format "%s.emacs/packages" cc4s-root-directory))
(setq user-emacs-directory (format "%s.emacs" cc4s-root-directory))

(cc4s-log "Manual root directory :: %s" cc4s-root-directory)

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(cc4s-log "Requiring package")
(require 'package)
(cc4s-log "Installed packages go to %s" package-user-dir)

(setq package-enable-at-startup t)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/"   )))

(cc4s-log "Initializing packages")
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))



(cc4s-log "\t- get use-package")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(cc4s-log "\t\t-Requiring use-package")
(eval-when-compile
  (require 'use-package))

(use-package! org
    :ensure t
    :config
    (setq org-src-fontify-natively t
          org-src-preserve-indentation t
          org-src-tab-acts-natively t)
    (setq org-confirm-babel-evaluate nil))
(require 'org)

(use-package! htmlize
  :defer t
  :ensure t)

(use-package! citeproc :defer t :ensure t)
(use-package! org-ref
    :defer t
    :ensure t
    :config
    (setq bibtex-completion-bibliography
          (list (format "%s/%s" cc4s-root-directory "group.bib")))
    (cc4s-log "Bib files: %s" bibtex-completion-bibliography))

(use-package! yaml-mode
  :defer t
  :ensure t)

(use-package! org-plus-contrib
  :defer t
  :ensure nil)

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

;; ORG-ID for id:links
(setq-default org-id-locations-file
              (concat cc4s-root-directory "id-locations"))
(defun cc4s/handle-id-links ()
  (require 'org-id)
  (if (file-exists-p org-id-locations-file)
      (progn (cc4s-log "loading id:links from %s" org-id-locations-file)
             (org-id-locations-load)
             (!!done))
    (progn (cc4s-log "Updating id: links")
           (org-id-update-id-locations
            (directory-files-recursively cc4s-root-directory
                                         ".org$"))
           (!!done))))

(defun cc4s/publish-site ()
  (interactive)
  (require 'cl-macs)
  (let* ((site-file-path cc4s-root-directory)
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
             :exclude ".emacs/*"
             :base-directory ,site-file-path
             :base-extension "org"
             :publishing-directory ,publish-directory
             :with-creator nil
             :with-author nil
             :section-numbers t
             :table-of-contents t
             ;; :publishing-function org-html-publish-to-html
             :publishing-function cc4s/publish-to-html
             ;; :publishing-function org-html-export-to-html
             :htmlized-source nil
             :html-validation-link nil
             :html-head-extra ,cc4s/html-head-libs
             :language en
             ;; :body-only t
             ;; :makeindex t
             ;; :html-use-infojs nil
             ;; :html-link-home "sitemap.html"
             :auto-sitemap t
             :html-preamble ,cc4s/navigation-bar
             :html-self-link-headlines t
             :sitemap-title "Sitemap"
             :exclude "config/*"
             :recursive t))))

    (cc4s-log "Publishing cc4s user manual")
    (cc4s-log "build directory %s" publish-directory)
    (cc4s/handle-id-links)
    (org-publish-all)
    (!!done)
    (cc4s-log "aGa: If you have problems with id:links erase \n\t%s"
              org-id-locations-file)))
