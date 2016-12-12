(require 'autopair)
(require 'evil)
(evil-mode 1)
(require 'evil-magit)
(require 'evil-leader)
(global-flycheck-mode)

(set-face-attribute 'default nil :family "Source Code Pro" :height 145 :weight 'normal)

(show-paren-mode t)
(setq show-paren-style 'mixed)
(autopair-global-mode)

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
(custom-set-faces)

(load-theme 'monokai t)

(set-face-attribute 'default nil :height 150)

(global-linum-mode t)
(global-evil-leader-mode)
(projectile-mode)
(add-hook 'neotree-mode-hook
        (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q")   'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(setq ensime-startup-notification nil)
(setq ensime-snapshot-notification nil)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'dark)
(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-buffer-max-length nil
      helm-recentf-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t
      helm-ff-auto-update-initial-value t
      helm-full-frame nil
      helm-split-window-in-side-p t)

(provide 'setup)
