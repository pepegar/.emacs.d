;;; 20-core-magit.el --- packages managements core
(use-package magit
  :straight t)

(use-package forge
  :straight t)

(use-package git-gutter
  :straight t
  :unless (version< emacs-version "26.1")
  :hook ((prog-mode text-mode) . git-gutter-mode)
  :diminish git-gutter-mode
  :custom
  (git-gutter:hide-gutter t)
  (git-gutter:update-interval 0.1)
  (git-gutter:verbosity 0))

(provide '20-core-magit)
;;; 20-core-magit.el ends here
