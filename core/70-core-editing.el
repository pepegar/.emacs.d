;;; 70-core-editing.el --- basic edit tools I use

(use-package paredit
  :straight t
  :config (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package multiple-cursors
  :straight t
  :bind (("C-* l" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-* C-*" . mc/mark-all-like-this)
         ("C-c C-* C-*" . mc/mark-more-like-this)
         ("C-* i" . mc/insert-numbers)
         ("C-* s" . mc/sort-regions)
         ("C-* r" . mc/reverse-regions)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :config
  (require 'mc-extras))

(use-package mc-extras
  :straight t
  :commands (mc/compare-chars mc/compare-chars-backward mc/compare-chars-forward
                              mc/cua-rectangle-to-multiple-cursors
                              mc/remove-current-cursor mc/remove-duplicated-cursors)
  :config
  (progn
    (bind-keys :map mc/keymap
               ("C-. C-d" . mc/remove-current-cursor)
               ("C-. d" . mc/remove-duplicated-cursors)
               ("C-. =" . mc/compare-chars))
    (eval-after-load 'cua-base
      '(bind-key "C-. C-," 'mc/cua-rectangle-to-multiple-cursors cua--rectangle-keymap))))

(use-package expand-region
  :straight t
  :bind ("C-@" . er/expand-region))

(use-package move-text
  :straight t
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(provide '70-core-editing)
;;; 70-core-editing.el ends here
