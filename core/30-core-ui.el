;;; 20-core-ui.el --- User Insterface core

(use-package all-the-icons
  :straight t
  :commands (all-the-icons-faicon))

(use-package doom-themes
  :straight t
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-Iosvkem t)
  (doom-themes-org-config))

(use-package dashboard
  :straight t
  :demand
  :if (< (length command-line-args) 2)
  :bind (:map dashboard-mode-map
              ("U" . auto-package-update-now)
              ("R" . restart-emacs)
              ("K" . kill-emacs))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title "The One True Editor, Emacs")
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info nil)
  (dashboard-set-navigator t)
  (dashboard-footer-icon (cond ((display-graphic-p)
                                (all-the-icons-faicon "code" :height 1.5 :v-adjust -0.1 :face 'error))
                               (t (propertize ">" 'face 'font-lock-doc-face))))
  :config
  (defun dashboard-load-packages (list-size)
    (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 2) ? )
            (format "[%d packages loaded in %s]" (length package-activated-list) (emacs-init-time))))

  (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages))

  (setq dashboard-items '((packages)
                          (projects . 10)
                          (recents . 10)))
  (dashboard-setup-startup-hook))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :bind ("C-x C-k" . kill-this-buffer)
  :config (menu-bar-mode -1))

(use-package tool-bar
  :ensure nil
  :config (tool-bar-mode -1))

(use-package whitespace
  :diminish global-whitespace-mode
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face tabs trailing)))

(use-package rainbow-mode
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(unless (version< emacs-version "26.1")
  (use-package display-line-numbers
    :hook ((prog-mode text-mode) . display-line-numbers-mode)))

(when (version< emacs-version "26.1")
  (use-package linum
    :hook ((prog-mode text-mode) . linum-mode)
    :custom
    (linum-format "%4d")))

(use-package zoom-window
  :straight t
  :bind (("C-x C-z" . zoom-window-zoom))
  :custom
  (zoom-window-mode-line-color "DarkRed" "Modeline color when enabled"))

(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window))
  :custom
  (aw-dispatch-always t "Issue read-char even for one window")
  (ace-window-display-mode t)
  :config
  (push " *NeoTree*" aw-ignored-buffers)
  (push "*which-key*" aw-ignored-buffers))

(use-package faces
  :ensure nil
  :config
  (when (member "PragmataPro" (font-family-list))
    (set-face-attribute 'default nil :font "PragmataPro Mono Liga 12"))
  (when (member "FontAwesome" (font-family-list))
    (set-fontset-font t 'unicode "FontAwesome" nil 'prepend)))

(use-package doom-modeline
  :straight t
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-height 20)
  (doom-modeline-major-mode-color-icon t))

(use-package golden-ratio
  :straight t
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(provide '30-core-ui)
;;; 30-core-ui.el ends here
