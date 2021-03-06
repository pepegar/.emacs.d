(use-package hydra
  :straight t
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

(provide 'module-hydra)
