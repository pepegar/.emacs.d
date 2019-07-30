(use-package org
  :bind (("C-c a a" . org-agenda)
       ("C-c c" . counsel-org-capture))
  :config

  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
               ;; beamer class, for presentations
               '("beamer"
                 "\\documentclass[11pt]{beamer}\n
        \\mode<{{{beamermode}}}>\n
        \\usetheme{{{{beamertheme}}}}\n
        \\usecolortheme{{{{beamercolortheme}}}}\n
        \\beamertemplateballitem\n
        \\setbeameroption{show notes}
        \\usepackage[utf8]{inputenc}\n
        \\usepackage[T1]{fontenc}\n
        \\usepackage{hyperref}\n
        \\usepackage{color}
        \\usepackage{listings}
        \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
    frame=single,
    basicstyle=\\small,
    showspaces=false,showstringspaces=false,
    showtabs=false,
    keywordstyle=\\color{blue}\\bfseries,
    commentstyle=\\color{red},
    }\n
        \\usepackage{verbatim}\n
        \\institute{{{{beamerinstitute}}}}\n
         \\subject{{{{beamersubject}}}}\n"

                 ("\\section{%s}" . "\\section*{%s}")

                 ("\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}"
                  "\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (mscgen . t)
     (python . t)
     (restclient . t)
     (haskell . t)))
  (setq org-agenda-files '("~/org/")
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

(provide 'module-org)
