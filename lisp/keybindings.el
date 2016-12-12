(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'helm-M-x
  "ft" 'neotree-toggle
  "ww" 'other-window
  "w/" 'split-window-right
  "w-" 'split-window-below
  "gs" 'magit-status
  )

(require 'scala-keybindings)

(provide 'keybindings)
