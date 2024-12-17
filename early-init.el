(require 'package)
(setq
  ;; archives to install packages from
  package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                     ("melpa" . "https://melpa.org/packages/"))
  ;; packages to have installed
  package-selected-packages '(
			      blacken
			      company
			      desktop
			      dirtree
			      flycheck
			      flycheck-pyflakes
			      go-autocomplete
			      lua-mode
			      magit
			      markdown-mode
			      neotree
			      outline-indent
			      popup
			      python-mode
			      rust-mode
			      tabbar
			      xml-format
                              git-gutter
			      ))
(package-initialize)
