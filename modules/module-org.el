(defvar org-path "~/org")
(defvar braindump-path "~/org/braindump/org")

(use-package org
  :bind (("C-c a a" . org-agenda)
         ("C-c c" . counsel-org-capture)))

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
  (org-roam-directory braindump-path)
  (org-roam-graph-executable "~/.nix-profile/bin/dot")
  (org-roam-completion-system 'ivy)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          )))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory braindump-path))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir org-path)
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox
  :config
  (org-hugo-auto-export-mode))

(provide 'module-org)
