(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :init
  (evil-mode +1)
  :config
  (evil-set-undo-system 'undo-tree))
