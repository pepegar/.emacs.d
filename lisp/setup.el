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

(defun my-load-saved-theme ()
  (interactive)
  (when (file-exists-p my-saved-theme-filename)
    (let ((theme (intern (with-temp-buffer
                           (insert-file-contents my-saved-theme-filename)
                           (buffer-string)))))
      (unless (eq theme 'default)
        (load-theme theme :no-confirm)))))

(add-hook 'after-init-hook #'my-load-saved-theme)

(defvar my-load-theme-hook
  nil
  "Hooks to run after loading a theme.")

(defvar my-saved-theme-filename "~/.emacs.d/.emacs-theme")

(advice-add 'load-theme :after #'my-save-theme)
(advice-add 'disable-theme :after #'my-save-default-theme)
(advice-add 'load-theme :after #'my-run-theme-hooks)

(defun my-run-theme-hooks (theme &optional no-confirm no-enable)
  (run-hooks 'my-load-theme-hook))

(defun my-save-default-theme (disabled-theme)
  (my-save-theme 'default))

(defun my-save-theme (theme &optional no-confirm no-enable)
  (with-temp-buffer
    (insert (symbol-name theme))
    (when (file-writable-p my-saved-theme-filename)
      (write-region (point-min)
                    (point-max)
                    my-saved-theme-filename))))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
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
