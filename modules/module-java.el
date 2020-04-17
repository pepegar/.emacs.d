;;; module-java.el --- java integration

(use-package lsp-java
  :ensure t
  :after lsp
  :hook (java-mode . lsp))

(provide 'module-java)
;;; module-java.el ends here
