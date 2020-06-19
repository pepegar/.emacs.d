(defvar org-path (concat (getenv "HOME") "/org"))
(defvar braindump-path (concat (getenv "HOME") "/org/braindump/org"))
(defvar zotero-library (concat (getenv "HOME") "/library.bib"))
(defvar note-template
  (concat
   "#+TITLE: ${citekey}: ${title}\n"
     "#+ROAM_KEY: ${ref}\n"
     "#+SETUPFILE:./hugo_setup.org\n"
     "#+HUGO_SECTION: zettels\n"
     "#+HUGO_SLUG: ${slug}\n"
     "\n"
     "* Notes\n"
     ":PROPERTIES:\n"
     ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n"
     ":NOTER_PAGE:\n"
     ":END:\n\n"))

(use-package org
  :straight t
  :bind (("C-c a a" . org-agenda)
         ("C-c c" . counsel-org-capture)))

(use-package ob-restclient
  :straight t
  :after org)

(use-package org-bullets
  :straight t
  :after org
  :commands (org-bullets-mode)
  :hook (org-mode-hook . org-bullets-mode))

(use-package org-present
  :straight t
  :after org)

(use-package org-roam
  :straight t
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
           :head (s-join "\n" '("#+SETUPFILE:./hugo_setup.org"
                                 "#+HUGO_SECTION: zettels"
                                 "#+HUGO_SLUG: ${slug}"
                                 "#+TITLE: ${title}"))
           :unnarrowed t)
          )))

(use-package deft
  :straight t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory braindump-path))

(use-package org-journal
  :straight t
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir org-path)
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-download
  :straight t
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package ox-hugo
  :straight t
  :ensure t
  :after ox
  :config
  (org-hugo-auto-export-mode))

(use-package pdf-tools
  :straight t
  :ensure t)

(use-package org-ref
  :straight t
  :ensure t
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography (list zotero-library)
   org-ref-bibliography-notes (concat braindump-path "bibnotes.org")
   org-ref-note-title-format (s-join "\n" '(
                                            "* TODO %y - %t"
                                            ":PROPERTIES:"
                                            ":Custom_ID: %k"
                                            ":NOTER_DOCUMENT: %F"
                                            ":ROAM_KEY: cite:%k"
                                            ":AUTHOR: %9a"
                                            ":JOURNAL: %j"
                                            ":YEAR: %y"
                                            ":VOLUME: %v"
                                            ":PAGES: %p"
                                            ":DOI: %D"
                                            ":URL: %U"
                                            ":END:"
                                            ))
   org-ref-notes-directory braindump-path
   org-ref-notes-function 'orb-edit-notes
   ))

(use-package bibtex-completion
  :straight t
  :ensure t)

(use-package org-roam-bibtex
  :straight t
  :requires bibtex-completion
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :load-path "~/projects/org-roam-bibtex/"
  :bind (:map org-roam-bibtex-mode-map
              (("C-c m f" . orb-find-non-ref-file))
              :map org-mode-map
              (("C-c m t" . orb-insert-non-ref)
               ("C-c m a" . orb-note-actions)))
  :custom
  (orb-templates
   `(("n" "ref + noter" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "${slug}"
      :head ""
      ;note-template
      ))))

(use-package ivy-bibtex
  :straight t
  :ensure t
  :bind* ("C-c C-r" . ivy-bibtex)
  :config
  (setq
   bibtex-completion-bibliography zotero-library
   bibtex-completion-notex-path braindump-path
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files "";note-template
   ))

(use-package org-noter
  :straight t
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)))

(provide 'module-org)
