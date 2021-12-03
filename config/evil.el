;; type only y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; good for discovery
(use-package which-key
  :ensure t
  :init
  (which-key-mode +1))

;; better undo for vim
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package ivy
  :ensure t
  :init
  (ivy-mode +1))

;; VIMMMMMMMMMMMMMMMMMMM ;)
(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :init
  (evil-mode +1)
  :config
  ;; some defaul bindings for the people
  (require 'org-ref)
  (evil-define-key 'normal global-map
    (kbd "SPC") nil
    (kbd "SPC l") #'org-latex-preview
    (kbd "SPC L") #'org-insert-link
    (kbd "SPC i") #'org-insert-structure-template
    (kbd "SPC c") #'org-ref-insert-cite-link
    (kbd "SPC C") #'org-ref-insert-link
    (kbd "SPC p") #'cc4s/publish-site)
  (evil-set-undo-system 'undo-tree))
