(setq user-full-name "Pepe García")
(setq user-mail-address "jlgarhdez@gmail.com")

(require 'cl)
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar pepegar/packages '(
                           helm
                           projectile
                           helm-projectile
                           clojure-mode
                           deft
                           erlang
                           flycheck
                           gist
                           haskell-mode
                           magit
                           markdown-mode
                           marmalade
                           org
                           paredit
                           restclient
                           scala-mode
                           ensime
                           evil
                           evil-leader
                           evil-magit
                           hydandata-light-theme
                           neotree
                           )
  "Default packages")

(defun pepegar/packages-installed-p ()
  (loop for pkg in pepegar/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (pepegar/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg pepegar/packages)
    (when (not (package-installed-p pkg))
            (package-install pkg))))

(require 'evil)
(evil-mode 1)
(require 'evil-magit)
(require 'evil-leader)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(setq make-backup-files nil
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'hydandata-light)


(set-face-attribute 'default nil :height 150)

(global-linum-mode t)
(global-evil-leader-mode)
(projectile-global-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'helm-M-x
  "ft" 'neotree-toggle
  )
