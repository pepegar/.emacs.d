;;; 40-core-ivy.el --- Modeline configuration


(use-package projectile
  :straight t
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'ivy))

(use-package ivy
  :straight t
  :diminish ivy-mode
  :bind (("C-x C-b" . ivy-switch-buffer))
  :config
      (setq ivy-use-virtual-buffers t
            ivy-count-format "%d/%d "
            ivy-re-builders-alist '((swiper . ivy--regex-plus))))

(use-package flx
  :straight t)

(use-package counsel
  :straight t
  :bind (("M-x"     . counsel-M-x)
         ([f9]      . counsel-load-theme))
  :config
    (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :straight t
  :bind (("C-c a g" . counsel-ag)
         ("C-x C-f" . counsel-find-file)
         ("C-c p h" . counsel-projectile)
         ("C-c p r" . projectile-replace)
         ("C-c p v" . projectile-vc)
         ("C-c p p" . counsel-projectile-switch-project)))

(use-package swiper
  :straight t
  :bind (("C-s" . swiper)
         ("M-l" . swiper-avy)))

(use-package ivy-posframe
  :straight t
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((left-fringe . 15)
                                  (right-fringe . 15)
                                  (top-fringe . 15)
                                  (bottom-fringe . 15)))
(ivy-posframe-mode 1))

(provide '40-core-ivy)
;;; 40-core-ivy.el ends here
