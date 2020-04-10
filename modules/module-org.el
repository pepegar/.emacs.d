(use-package org
  :bind (("C-c a a" . org-agenda)
         ("C-c c" . counsel-org-capture))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (mscgen . t)
     (python . t)
     (restclient . t)
     (haskell . t)))
  (setq org-agenda-files '("~/Dropbox/org/")
        org-capture-templates '(("t" "To Do Item" entry (file+headline "~/org/i.org" "Work") "* TODO %?\n%T" :prepend t)
                                ("o" "opensource" entry (file+headline "~/org/i.org" "Opensource") "* TODO %?\n%T" :prepend t)
                                ("p" "Personal To Do Item" entry (file+headline "~/org/i.org" "Personal") "* TODO %?\n%T" :prepend t))
        org-src-fontify-natively t))


(use-package ob-restclient
  :after org)

(use-package org-bullets
  :after org
  :commands (org-bullets-mode)
  :hook (org-mode-hook . org-bullets-mode))

(use-package org-present
  :after org)

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox/org")
  (org-roam-graph-executable "~/.nix-profile/bin/dot")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package company-org-roam
  :config
  (push 'company-org-roam company-backends))

(provide 'module-org)
