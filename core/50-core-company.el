;;; 50-core-company.el --- company integration

(use-package company
  :straight t
  :diminish
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 0)
  (company-tooltip-limit 12)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t))



(unless (version< emacs-version "26.1")
  (use-package company-box
    :straight t
    :diminish
    :after (company)
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-show-single-candidate t)
    (company-box-max-candidates 50)
    (company-box-doc-delay 0.2)
    (company-box-icons-alist 'company-box-icons-all-the-icons)))


(provide '50-core-company)
;;; 50-core-company.el ends here
