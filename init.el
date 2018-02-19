(package-initialize)

(setq user-full-name "Pepe García")
(setq user-mail-address "jl.garhdez@gmail.com")

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/pepe/.local/bin")))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'packages)
(require 'setup)

(load "~/.emacs.d/lisp/greek.el")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
