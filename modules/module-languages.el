(use-package haskell-mode :mode "\\.hs\\'")
(use-package idris-mode)
(use-package groovy-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package dhall-mode
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

(provide 'module-languages)
