(require 'cl)
(load "package")
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
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
                           evil
                           evil-leader
                           evil-magit
                           monokai-theme
                           neotree
                           flycheck
                           restclient
                           smart-mode-line
                           markdown-toc
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

(provide 'packages)
