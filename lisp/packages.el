(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
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
  :ensure t
  :diminish autopair-mode)

(use-package helm
  :ensure t
  :bind* (
	  ("C-c p p" . helm-projectile-switch-project)
	  ("C-c p h" . helm-projectile)
	  ("M-x" . helm-M-x)
	  ("C-x C-b" . helm-buffers-list)
	  ("C-h b" . helm-descbinds)
	  )
  :config
  (use-package helm-projectile
    :ensure t)

  (use-package helm-descbinds
    :ensure t)

  (use-package helm-themes
    :ensure t
    :if (display-graphic-p)
    :bind ([f9] . helm-themes))

  (use-package helm-ag
    :ensure t
    :bind ("C-c a g" . helm-do-ag-project-root))

  (use-package helm-swoop
    :ensure t
    :demand isearch
    :bind (("M-i" . helm-swoop)
           ("M-I" . helm-multi-swoop)
           :isearch-mode-map
           ("M-i" . helm-swoop-from-isearch)))
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode)

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode))

(use-package what-the-commit
  :ensure t
  :bind ("C-x g c" . what-the-commit-insert))

(use-package cus-edit
  :config
  (setq custom-file (make-temp-file "")))


(use-package clojure-mode
  :ensure t)

(use-package deft
  :ensure t)

(use-package flycheck
  :diminish flycheck-mode
  :ensure t)

(use-package gist
  :ensure t)

(use-package idris-mode
  :ensure t
  :pin melpa-stable)

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
  :ensure t
  :config

  (use-package magit-gh-pulls
    :ensure t
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

 (use-package hydra
  :ensure t
  :bind (("C-x t" . toggle/body)
	 ("C-x j" . gotoline/body)
	 ("C-x c" . orghydra/body)
	 ("C-x p" . dotfiles/body))
  :config

  (defhydra toggle (:color blue)
    "toggle"
    ("a" abbrev-mode "abbrev")
    ("s" flyspell-mode "flyspell")
    ("f" flycheck-mode "flycheck")
    ("d" toggle-debug-on-error "debug")
    ("c" fci-mode "fCi")
    ("t" toggle-truncate-lines "truncate")
    ("w" whitespace-mode "whitespace")
    ("q" nil "cancel"))

  (defhydra orghydra (:color blue)
    "org"
    ("i" org-clock-in "clock in")
    ("o" org-clock-out "clock out")
    ("n" (find-file "~/org/notes.org") "notes.org")
    ("I" (find-file "~/org/i.org") "i.org")
    ("q" nil "cancel"))

  (defhydra dotfiles (:color black)
    "dotfiles"
    ("e" (find-file "~/.emacs.d/init.el") "init.el")
    ("p" (find-file "~/.emacs.d/lisp/packages.el") "packages.el")
    ("g" (find-file "~/.emacs.d/lisp/greek.el") "greek.el")
    ("s" (find-file "~/.emacs.d/lisp/setup.el") "setup.el")
    ("z" (find-file "~/.zshrc") "zshrc")
    ("q" nil "cancel"))

  (defhydra gotoline
    ( :pre (linum-mode 1)
	   :post (linum-mode -1))
    "goto"
    ("t" (lambda () (interactive)(move-to-window-line-top-bottom 0)) "top")
    ("b" (lambda () (interactive)(move-to-window-line-top-bottom -1)) "bottom")
    ("m" (lambda () (interactive)(move-to-window-line-top-bottom)) "middle")
    ("e" (lambda () (interactive)(end-of-buffer)) "end")
    ("c" recenter-top-bottom "recenter")
    ("n" next-line "down")
    ("p" (lambda () (interactive) (forward-line -1))  "up")
    ("g" goto-line "goto-line")))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :ensure t
  :bind (("C-c a a" . org-agenda)
	 ("C-c c" . org-capture))
  :config

  (use-package org-bullets
    :ensure t
    :commands (org-bullets-mode)
    :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (mscgen . t)
     (python . t)))
  (setq org-agenda-files '("~/org/")
	org-default-notes-file (concat org-directory "/notes.org")
	org-capture-templates '(("a" "Appointment" entry (file  "~/org/appointments.org" "Appointments") "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n\n")
				("n" "Note" entry (file+headline "~/org/notes.org" "Notes") "* Note %?\n%T")
				("l" "Link" entry (file+headline "~/org/links.org" "Links") "* %? %^L %^g \n%T" :prepend t)
				("P" "Paper" entry (file+headline "~/org/papers.org" "Papers") "* %? %^L %^g \n%T" :prepend t)
				("t" "To Do Item" entry (file+headline "~/org/i.org" "Work") "* TODO %?\n%T" :prepend t)
				("p" "Personal To Do Item" entry (file+headline "~/org/i.org" "Personal") "* TODO %?\n%T" :prepend t))
	org-src-fontify-natively t))

(use-package paredit
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package scala-mode
  :ensure t)

(use-package groovy-mode
  :ensure t
  :mode ("\\.gradle\\'" . groovy-mode))

(use-package nix-mode
  :ensure t)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package doom-themes :pin melpa-stable :ensure t :defer t)
(use-package idea-darkula-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package punpun-theme :ensure t :defer t)
(use-package white-theme :ensure t :defer t)
(use-package arjen-grey-theme :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package birds-of-paradise-plus-theme :ensure t :defer t)
(use-package bliss-theme :ensure t :defer t)
(use-package borland-blue-theme :ensure t :defer t)
(use-package cyberpunk-theme :ensure t :defer t)
(use-package django-theme :ensure t :defer t)
(use-package eclipse-theme :ensure t :defer t)
(use-package espresso-theme :ensure t :defer t)
(use-package faff-theme :ensure t :defer t)
(use-package github-theme :ensure t :defer t)
(use-package greymatters-theme :ensure t :defer t)
(use-package heroku-theme :ensure t :defer t)
(use-package idea-darkula-theme :ensure t :defer t)
(use-package plan9-theme :ensure t :defer t)
(use-package purple-haze-theme :ensure t :defer t)
(use-package railscasts-theme :ensure t :defer t)
(use-package rebecca-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package soothe-theme :ensure t :defer t)
(use-package subatomic-theme :ensure t :defer t)
(use-package sublime-themes :ensure t :defer t)
(use-package white-theme :ensure t :defer t)
(use-package madhat2r-theme :ensure t :defer t)
(use-package kosmos-theme :ensure t :defer t)
(use-package nord-theme :ensure t :defer t)

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle)
	 ))

(use-package flycheck
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind (("M-RET" . dumb-jump-go)))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package restclient
  :ensure t)

(use-package markdown-toc
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-* l" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
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
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init
  (use-package elm-yasnippets :ensure t)

  (add-hook 'js2-mode-hook 'yas-minor-mode)
  (add-hook 'haskell-mode-hook 'yas-minor-mode)
  (add-hook 'elm-mode-hook 'yas-minor-mode)

  :config
  (yas-reload-all)

  :bind (("C-t" . yas-expand)))

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

(use-package geiser
  :ensure t)

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :commands (json-mode)
  :config
  (setq js-indent-level 2))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :commands haskell-mode
  :bind (("C-c C-s" . fix-imports)
	 ("C-c C-c" . haskell-compile)
	 ("C-,"     . haskell-move-nested-left)
	 ("C-."     . haskell-move-nested-right)
	 ("C-c C-." . haskell-mode-format-imports)
	 ("s-i"     . haskell-navigate-imports)
	 ("C-c C-l" . haskell-process-load-or-reload)
	 ("C-`"     . haskell-interactive-bring)
	 ("C-c C-t" . haskell-process-do-type)
	 ("C-c C-i" . haskell-process-do-info)
	 ("C-c C-c" . haskell-process-cabal-build)
	 ("C-c C-k" . haskell-interactive-mode-clear)
	 ("C-c c"   . haskell-process-cabal))
  :config
  (custom-set-variables
   '(haskell-ask-also-kill-buffers nil)
   '(haskell-interactive-popup-errors nil)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-suggest-remove-import-lines  t)
   '(haskell-process-type (quote stack-ghci))
   '(haskell-stylish-on-save t))


  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook (lambda ()
                                 (add-hook 'before-save-hook 'haskell-mode-format-imports nil 'local))))

(use-package github-browse-file
  :ensure t)

(use-package fancy-narrow
  :ensure t
  :diminish fancy-narrow-mode
  :config (fancy-narrow-mode))

(use-package yaml-mode
  :ensure t)

(use-package smart-window
  :ensure t)

(use-package git-gutter
  :ensure t)

(provide 'packages)
