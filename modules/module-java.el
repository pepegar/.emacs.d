;;; module-java.el --- java integration

(use-package lsp-java
  :mode "\\.(java|gradle)$"
  :hook (java-mode . lsp))

(provide 'module-java)
;;; module-java.el ends here
