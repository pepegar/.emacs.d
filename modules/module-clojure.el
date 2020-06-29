;;; module-clojure.el --- clojure integration

(use-package clojure-mode
  :straight t
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook ((clojure-mode . yas-minor-mode)
         (clojure-mode . linum-mode)
         (clojure-mode . subword-mode)
         (clojure-mode . smartparens-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . eldoc-mode)
         (clojure-mode . idle-highlight-mode)))

(use-package idle-highlight-mode
  :straight t)

(use-package cider
  :straight t
  :ensure t
  :defer t
  :hook (cider-mode . clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu
  :straight t
  :defer t)

(use-package clj-refactor
  :straight t
  :defer t
  :ensure t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package smartparens
  :straight t
  :defer t
  :ensure t
  :diminish smartparens-mode
  :init
  (setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-)" . sp-forward-slurp-sexp)
          ("M-<backspace>" . nil)
          ("C-(" . sp-forward-barf-sexp)))
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode))

(provide 'module-clojure)
;;; module-clojure.el ends here
