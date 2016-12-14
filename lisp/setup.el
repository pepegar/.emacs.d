(require 'autopair)
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

(load-theme 'monokai t)

(set-face-attribute 'default nil :height 150)

(global-linum-mode t)
(projectile-mode)

(setq ensime-startup-notification nil)

(setq ensime-snapshot-notification nil)

(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'dark)

(setq helm-split-window-in-side-p t
      helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-buffer-max-length nil
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t
      helm-ff-auto-update-initial-value t
      helm-full-frame nil
      helm-split-window-in-side-p t
      helm-M-x-fuzzy-match t
      )

(provide 'setup)
