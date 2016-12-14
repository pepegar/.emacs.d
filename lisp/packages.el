(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package autopair
  :ensure t)

(use-package helm
  :ensure t
  :bind* (
	  ("C-c p p" . helm-projectile-switch-project)
	  ("C-c p h" . helm-projectile)
	  ("M-x" . helm-M-x)
	  ))

(use-package projectile
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package deft
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package gist
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package hungry-delete
  :ensure t
  :defer t
  :diminish hungry-delete-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'hungry-delete-mode)
  (add-hook 'haskell-mode-hook 'hungry-delete-mode)
  (add-hook 'scala-mode-hook 'hungry-delete-mode))

(use-package ns-win
  :if (eq system-type 'darwin)
  :config
  (setq mac-command-modifier 'meta
        mac-control-modifier 'control
        mac-right-option-modifier 'control))

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package org
  :ensure t)

(use-package paredit
  :ensure t)

(use-package restclient
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package ensime
  :ensure t)

(use-package monokai-theme
  :ensure t)

(use-package neotree
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package restclient
  :ensure t)

(use-package smart-mode-line
  :ensure t)

(use-package markdown-toc
  :ensure t)

(provide 'packages)
