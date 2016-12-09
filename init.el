(setq user-full-name "Pepe García")
(setq user-mail-address "jlgarhdez@gmail.com")

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/home/abedra/.cabal/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/src/golang"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(require 'cl)

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

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
                           evil)
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

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c p h") 'helm-projectile)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(projectile-global-mode)

