(use-package nix-mode
  :straight t
  :commands nix-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))
  :hook (nix-mode-hook .rainbow-delimiters-mode))

(provide 'module-nix)
