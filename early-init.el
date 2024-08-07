(require 'package)
(setq
  ;; archives to install packages from
  package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                     ("melpa" . "https://melpa.org/packages/"))
  ;; packages to have installed
  package-selected-packages '(magit
                              git-gutter
			      xml-format
			      flycheck
			      flycheck-pyflakes
			      lua-mode
			      neotree
			      tabbar
			      dirtree
			      markdown-mode
			      ))
(package-initialize)
