(require 'autopair)
(global-flycheck-mode)

(set-face-attribute 'default nil :family "PragmataPro" :height 155 :weight 'normal)

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

(load-theme 'punpun-dark t)
(set-face-attribute 'default nil :height 140)

(global-linum-mode t)
(projectile-mode)

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
