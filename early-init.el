(require 'package)
(setq
  ;; archives to install packages from
  package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                     ("melpa" . "https://melpa.org/packages/"))
  ;; packages to have installed
  package-selected-packages '(
			      blacken
			      desktop
			      dirtree
			      flycheck
			      flycheck-pyflakes
			      go-autocomplete
			      lua-mode
			      magit
			      markdown-mode
			      outline-indent
			      popup
			      python-mode
			      rust-mode
			      tabbar
			      xml-format
			      yaml-mode
                              git-gutter
			      pyvenv
			      treemacs
			      corfu
			      eglot
			      ))
(package-initialize)
