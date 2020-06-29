;;; module-haskell.el --- haskell integration

(use-package haskell-mode
  :straight t
  :custom
  (haskell-stylish-on-save t)
  :hook (haskell-mode . lsp))

(use-package lsp-haskell
  :straight t
  :after (haskell))

(provide 'module-haskell)
;;; module-haskell.el ends here
