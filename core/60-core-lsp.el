;;; 60-core-lsp.el --- lsp support

(use-package lsp-ui
  :straight t
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-include-signture t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-sideline-enable nil)
  :commands lsp-ui-mode)

(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ((scala-mode haskell-mode) . lsp-deferred))
  :custom
  (lsp-session-file (concat cache-dir ".lsp-session-v1"))
  (lsp-prefer-flymake t)
  (lsp-enable-snippet nil)
  :commands lsp)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

(use-package company-lsp
  :straight t
  :after company-mode
  :custom
  (company-lsp-cache-candidates nil)
  (company-lsp-async t)
  (company-lsp-enable-recompletion t))

(use-package dap-mode
  :straight t
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide '60-core-lsp)
;;; 60-core-lsp.el ends here
