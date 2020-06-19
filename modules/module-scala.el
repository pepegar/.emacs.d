;;; module-scala.el --- scala integration

(use-package scala-mode
  :straight t
  :mode "\\.s\\(cala\\|bt\\)$"
  :custom
  (scala-indent:align-forms t)
  (scala-indent:align-parameters t)
  (scala-indent:indent-value-expression t)
  (scala-indent:default-run-on-strategy)
  (scala-indent:operator-strategy))

(use-package sbt-mode
  :straight t
  :after scala-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-metals
  :straight t)

(provide 'module-scala)
;;; module-scala.el ends here
