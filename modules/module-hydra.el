(use-package hydra
  :bind (("C-x t" . toggle/body)
         ("C-x j" . gotoline/body)
         ("C-x c" . orghydra/body)
         ("C-x p" . dotfiles/body))
  :custom
  (hydra-hint-display-type 'posframe)
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
    ("c" (find-file "~/.emacs.d/config.org") "config.org")
    ("C" (find-file "/su::/etc/nixos/configuration.nix") "configuration.nix")
    ("h" (find-file "~/.config/nixpkgs/home.nix") "home.nix")
    ("z" (find-file "~/.zshrc") "zshrc")
    ("g" (find-file "~/.emacs.d/gnus.org") "gnus")
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

(use-package major-mode-hydra
  :ensure t
  :bind ("M-SPC" . major-mode-hydra)
  :config
  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat "\n"
                     (s-repeat 10 " ")
                     (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                     " "
                     (symbol-name mode)
                     " commands")))

  (major-mode-hydra-define emacs-lisp-mode nil
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Test"
     (("t" ert "prompt")
      ("T" (ert t) "all")
      ("F" (ert :failed) "failed"))
     "Doc"
     (("d" describe-foo-at-point "thing-at-pt")
      ("f" describe-function "function")
      ("v" describe-variable "variable")
      ("i" info-lookup-symbol "info lookup")))))

(provide 'module-hydra)
