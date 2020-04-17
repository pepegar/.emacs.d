;;; 60-core-lsp.el --- lsp support

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-include-signture t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-sideline-enable nil))

(use-package lsp-mode
  :custom
  (lsp-session-file (concat cache-dir ".lsp-session-v1"))
  (lsp-prefer-flymake t)
  (lsp-enable-snippet nil))

(use-package company-lsp
  :after company-mode
  :custom
  (company-lsp-cache-candidates nil)
  (company-lsp-async t)
  (company-lsp-enable-recompletion t))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide '60-core-lsp)
;;; 60-core-lsp.el ends here
