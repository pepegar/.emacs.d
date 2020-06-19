(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'")

(use-package idris-mode
  :straight t)

(use-package groovy-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package json-mode
  :straight t)

(use-package dhall-mode
  :straight t
  :mode  "\\.dhall\\'")

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")

  :config
  (use-package markdown-toc :straight t))

(use-package rjsx-mode
  :straight t
  :ensure t
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-bounce-indent-p nil))

(use-package rust-mode
  :straight t
  :ensure t)

(provide 'module-languages)
