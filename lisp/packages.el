(require 'package)
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
	  ("C-x C-b" . helm-buffers-list)
	  ))

(use-package projectile
  :ensure t)


(use-package cus-edit
  :config
  (setq custom-file (make-temp-file "")))

(use-package helm-projectile
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package deft
  :ensure t)

(use-package flycheck
  :diminish flycheck-mode
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
  :ensure t
  :mode "\\.http\\'")

(use-package scala-mode
  :ensure t)

(use-package ensime
  :ensure t
  :bind ([f10] . ensime-reload)
  :config
  (setq ensime-startup-notification nil
	ensime-startup-snapshot-notification nil))

(use-package monokai-theme
  :ensure t)

(use-package punpun-theme
  :ensure t)

(use-package white-theme
  :ensure t)

(use-package arjen-grey-theme
  :ensure t)

(use-package helm-themes
    :ensure t
    :if (display-graphic-p)
    :bind ([f9] . helm-themes))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle)
	 ))

(use-package flycheck
  :ensure t)

(use-package restclient
  :ensure t)

(use-package markdown-toc
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-* l" . mc/edit-lines)
	 ("C-* n" . mc/mark-next-like-this)
	 ("C-* p" . mc/mark-previous-like-this)
	 ("C-* C-*" . mc/mark-all-like-this)
	 ("C-c C-* C-*" . mc/mark-more-like-this)

	 ("C-* i" . mc/insert-numbers)
	 ("C-* s" . mc/sort-regions)
	 ("C-* r" . mc/reverse-regions)
	 ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :config
  (require 'mc-extras))

(use-package mc-extras
  :ensure t
  :commands (mc/compare-chars mc/compare-chars-backward mc/compare-chars-forward
			      mc/cua-rectangle-to-multiple-cursors
			      mc/remove-current-cursor mc/remove-duplicated-cursors)
  :config
  (progn
    (bind-keys :map mc/keymap
	       ("C-. C-d" . mc/remove-current-cursor)
	       ("C-. d" . mc/remove-duplicated-cursors)
	       ("C-. =" . mc/compare-chars))
    (eval-after-load 'cua-base
      '(bind-key "C-. C-," 'mc/cua-rectangle-to-multiple-cursors cua--rectangle-keymap))))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode)
  :init (progn (add-hook 'prog-mode-hook #'yas-minor-mode))
  :config (progn (yas-reload-all)))

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

(use-package geiser
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (progn
    (use-package intero
      :ensure t
      :config
      (progn 
        (add-hook 'haskell-mode-hook 'intero-mode)))))

(use-package helm-ag
  :ensure t
  :bind ("C-c a g" . helm-do-ag-project-root))

(use-package github-browse-file
  :ensure t)


(provide 'packages)