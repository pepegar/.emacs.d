(require 'evil)
(evil-mode 1)
(require 'evil-magit)
(require 'evil-leader)

(setq make-backup-files nil
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" default))))
(custom-set-faces
 )
(load-theme 'hydandata-light)

(set-face-attribute 'default nil :height 150)

(global-linum-mode t)
(global-evil-leader-mode)
(projectile-global-mode)
(add-hook 'neotree-mode-hook
        (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(provide 'setup)
