;;
;; This file is adapted from @danielmai's ~init.el~
;;
(setq gc-cons-threshold 400000000)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Set up package
(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))

(setq user-full-name "Pepe García"
      user-mail-address "jl.garhdez@gmail.com")

(setq exec-path (append exec-path '("/usr/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/pepe/.local/bin")))
(setq exec-path (append exec-path '("/Users/pepe/.nix-profile/bin")))
(setq exec-path (append exec-path '("/nix/var/nix/profiles/default/bin/")))
(setq exec-path (append exec-path '("/run/current-system/sw/bin")))

(setq backup-directory-alist `(("." . "~/.backups")))


(unless (version< emacs-version "26.1")
  (use-package display-line-numbers
    :hook ((prog-mode text-mode) . display-line-numbers-mode)))

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

(require 'bind-key)

(setq use-package-always-defer t
      use-package-always-ensure t)

(global-font-lock-mode 1)

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

(use-package faces
  :ensure nil
  :config
  (when (member "PragmataPro" (font-family-list))
    (set-face-attribute 'default nil :font "PragmataPro 12")))

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t))

(use-package move-text
  :config (move-text-default-bindings))

(use-package company
  :bind (("M-n" . company-complete)))

(use-package notmuch)

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package erc
  :config
  (setq erc-modules '(autojoin notifications)))

(use-package magit)

(use-package forge)

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'ivy))

(use-package diminish
  :pin melpa-stable)

(use-package posframe)

(use-package ivy
  :diminish ivy-mode
  :bind (("C-x C-b" . ivy-switch-buffer))
  :custom
    (hydra-hint-display-type 'posframe)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-re-builders-alist '((swiper . ivy--regex-plus))))

(use-package ivy-posframe
  ;; :after ivy
  :defer t
  :config
  ;; (setq ivy-display-function #'ivy-posframe-display)
  (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
  (ivy-posframe-enable))

(use-package flx)

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ([f9]      . counsel-load-theme))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :bind (("C-c a g" . counsel-ag)
         ("C-x C-f" . counsel-find-file)
         ("C-c p h" . counsel-projectile)
         ("C-c p r" . projectile-replace)
         ("C-c p v" . projectile-vc)
         ("C-c p p" . counsel-projectile-switch-project)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("M-l" . swiper-avy)))

(use-package hydra
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
    ("i" (find-file "~/.config/nixpkgs/applications/emacs/init.el") "init.el")
    ("C" (find-file "/su::/etc/nixos/configuration.nix") "configuration.nix")
    ("h" (find-file "~/.config/nixpkgs/home.nix") "home.nix")
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

(use-package hydra-posframe
  :load-path "~/.emacs.d/lisp/hydra-posframe.el"
  :hook (after-init . hydra-posframe-enable))

(use-package major-mode-hydra
  :ensure t
  :bind
  ("M-SPC" . major-mode-hydra))

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

(use-package org
  :bind (("C-c a a" . org-agenda)
	 ("C-c c" . counsel-org-capture))
  :config

  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
	       ;; beamer class, for presentations
	       '("beamer"
		 "\\documentclass[11pt]{beamer}\n
        \\mode<{{{beamermode}}}>\n
        \\usetheme{{{{beamertheme}}}}\n
        \\usecolortheme{{{{beamercolortheme}}}}\n
        \\beamertemplateballitem\n
        \\setbeameroption{show notes}
        \\usepackage[utf8]{inputenc}\n
        \\usepackage[T1]{fontenc}\n
        \\usepackage{hyperref}\n
        \\usepackage{color}
        \\usepackage{listings}
        \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
    frame=single,
    basicstyle=\\small,
    showspaces=false,showstringspaces=false,
    showtabs=false,
    keywordstyle=\\color{blue}\\bfseries,
    commentstyle=\\color{red},
    }\n
        \\usepackage{verbatim}\n
        \\institute{{{{beamerinstitute}}}}\n          
         \\subject{{{{beamersubject}}}}\n"
		 
		 ("\\section{%s}" . "\\section*{%s}")
		 
		 ("\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}"
		  "\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (mscgen . t)
     (python . t)
     (restclient . t)
     (haskell . t)))
  (setq org-agenda-files '("~/org/")
	org-capture-templates '(("t" "To Do Item" entry (file+headline "~/org/i.org" "Work") "* TODO %?\n%T" :prepend t)
				("o" "opensource" entry (file+headline "~/org/i.org" "Opensource") "* TODO %?\n%T" :prepend t)
				("p" "Personal To Do Item" entry (file+headline "~/org/i.org" "Personal") "* TODO %?\n%T" :prepend t))
	org-src-fontify-natively t))


(use-package ob-restclient
  :after org)

(use-package org-bullets
  :after org
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-present
  :after org)

(use-package multiple-cursors
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

(use-package expand-region
  :bind ("C-@" . er/expand-region))

(use-package avy)

(use-package yasnippet
  :demand
  :diminish
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :config
  (yas-global-mode t)
  (yas-reload-all))

(use-package yasnippet-snippets
  :pin melpa-stable
  :after yasnippet
  :config
  (yas-reload-all))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-height 20)
  (doom-modeline-major-mode-color-icon t))

(use-package dashboard
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
  (dashboard-set-footer t)
  :config
  (dashboard-setup-startup-hook))

(use-package xresources-theme :pin melpa)
(use-package doom-themes :pin melpa-stable)
(use-package spacemacs-theme :pin melpa)
(use-package idea-darkula-theme)
(use-package punpun-theme)
(use-package white-theme)
(use-package arjen-grey-theme)
(use-package atom-one-dark-theme)
(use-package birds-of-paradise-plus-theme)
(use-package bliss-theme)
(use-package cyberpunk-theme)
(use-package espresso-theme)
(use-package github-theme)
(use-package heroku-theme)
(use-package idea-darkula-theme)
(use-package plan9-theme)
(use-package soothe-theme)
(use-package subatomic-theme)
(use-package sublime-themes)
(use-package white-theme)
(use-package madhat2r-theme)
(use-package kosmos-theme)
(use-package nord-theme)

(use-package haskell-mode
  :mode "\\.hs$"
  :config
  (load "haskell-mode-autoloads" nil t))

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init (setq lsp-prefer-flycheck nil)
  :hook (scala-mode . lsp))

(use-package lsp-ui
  :custom (lsp-ui-sideline-enable nil)
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package idris-mode)

(use-package nix-mode
  :commands nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))
  :hook (nix-mode-hook .rainbow-delimiters-mode))

(use-package groovy-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package reformatter
  :pin melpa-stable)

(use-package dhall-mode
  :pin melpa
  :mode  "\\.dhall\\'")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")

  :config
  (use-package markdown-toc))


(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-bounce-indent-p nil))

(require 'diminish)
(require 'bind-key)

(electric-pair-mode 1)

(global-set-key (kbd "M-g a") "α") ; alpha
(global-set-key (kbd "M-g b") "β") ; beta
(global-set-key (kbd "M-g g") "γ") ; gamma
(global-set-key (kbd "M-g d") "δ") ; delta
(global-set-key (kbd "M-g e") "ε") ; epsilon
(global-set-key (kbd "M-g z") "ζ") ; zeta
(global-set-key (kbd "M-g h") "η") ; eta
(global-set-key (kbd "M-g q") "θ") ; theta
(global-set-key (kbd "M-g i") "ι") ; iota
(global-set-key (kbd "M-g k") "κ") ; kappa
(global-set-key (kbd "M-g l") "λ") ; lambda
(global-set-key (kbd "M-g m") "μ") ; mu
(global-set-key (kbd "M-g n") "ν") ; nu
(global-set-key (kbd "M-g x") "ξ") ; xi
(global-set-key (kbd "M-g o") "ο") ; omicron
(global-set-key (kbd "M-g p") "π") ; pi
(global-set-key (kbd "M-g r") "ρ") ; rho
(global-set-key (kbd "M-g s") "σ") ; psi
(global-set-key (kbd "M-g t") "τ") ; tau
(global-set-key (kbd "M-g u") "υ") ; upsilon
(global-set-key (kbd "M-g f") "ϕ") ; phi
(global-set-key (kbd "M-g j") "φ") ; phi
(global-set-key (kbd "M-g c") "χ") ; xi
(global-set-key (kbd "M-g y") "ψ") ; psi
(global-set-key (kbd "M-g w") "ω") ; omega
(global-set-key (kbd "M-g A") "Α") ; ALPHA
(global-set-key (kbd "M-g B") "Β") ; BETA
(global-set-key (kbd "M-g G") "Γ") ; GAMMA
(global-set-key (kbd "M-g D") "Δ") ; DELTA
(global-set-key (kbd "M-g E") "Ε") ; EPSILON
(global-set-key (kbd "M-g Z") "Ζ") ; ZETA
(global-set-key (kbd "M-g H") "Η") ; ETA
(global-set-key (kbd "M-g Q") "Θ") ; THETA
(global-set-key (kbd "M-g I") "Ι") ; IOTA
(global-set-key (kbd "M-g K") "Κ") ; KAPPA
(global-set-key (kbd "M-g L") "Λ") ; LAMBDA
(global-set-key (kbd "M-g M") "Μ") ; MU
(global-set-key (kbd "M-g N") "Ν") ; NU
(global-set-key (kbd "M-g X") "Ξ") ; XI
(global-set-key (kbd "M-g O") "Ο") ; OMICRON
(global-set-key (kbd "M-g P") "Π") ; PI
(global-set-key (kbd "M-g R") "Ρ") ; RHO
(global-set-key (kbd "M-g S") "Σ") ; PSI
(global-set-key (kbd "M-g T") "Τ") ; TAU
(global-set-key (kbd "M-g U") "Υ") ; UPSILON
(global-set-key (kbd "M-g F") "Φ") ; PHI
(global-set-key (kbd "M-g J") "Φ") ; PHI
(global-set-key (kbd "M-g C") "Χ") ; XI
(global-set-key (kbd "M-g Y") "Ψ") ; PSI
(global-set-key (kbd "M-g W") "Ω") ; OMEGA
(global-set-key (kbd "M-g .") "∘")
(global-set-key (kbd "M-g *") "⊛")
