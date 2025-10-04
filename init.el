(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'hyper)

(let ((default-directory "~/.emacs.d/site-lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

;; (setq package-check-signature nil)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(require 'column-marker)

(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

(use-package outline-indent
  :commands (outline-indent-minor-mode
	     outline-indent-insert-heading)
  :hook ((python-mode . outline-indent-minor-mode)
         (python-ts-mode . outline-indent-minor-mode))
  :custom (outline-indent-ellipsis " ▼ "))

(defun set-python-tabs ()
     (setq tab-width 4)
     (setq python-indent-offset 4)
     (setq indent-tabs-mode t)
     (setq py-indent-tabs-mode t))
(add-hook 'python-mode-hook 'set-python-tabs)
(add-hook 'python-mode-hook 'tabify (point-min) (point-max))
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 81)))

(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

(require 'blacken)
(add-hook 'python-mode-hook 'blacken-mode)

(require 'rust-mode)

(require 'elixir-ts-mode)
(setq auto-mode-alist
      (cons '("\\.ex$" . elixir-ts-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.exs$" . elixir-ts-mode) auto-mode-alist))

(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

(global-set-key [(control \.)] 'goto-line)
(global-set-key [(control \,)] 'compile)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-]" 'help-for-help)
(global-set-key (kbd "C-/") 'comment-line)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "ESC") 'company-abort))

(global-auto-revert-mode)

;; (electric-pair-mode)
;; (setq-default electric-pair-inhibit-predicate
;; 	      (lambda (c)
;; 		(if (looking-at "[ \n\t]")
;; 		    (electric-pair-default-inhibit c)
;; 		  t)))
;; (setq-default electric-pair-preserve-balance 1)

;; (setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
;; (setq electric-pair-inhibit-predicate
;; 	(lambda (c)
;; 	  (if (char-equal c ?\") t (electric-pair-default-inhibit c))))

(setq default-frame-alist '((font . "7x14")))
(set-background-color "black")
(set-foreground-color "white")
(set-face-background 'default' "black")
(set-face-foreground 'default' "white")

;;(setq default-tab-width 4)
;;(setq-default indent-tabs-mode t)
(setq-default transient-mark-mode t)
(setq-default auto-fill-mode t)
(setq-default fill-column 79)
(setq-default show-trailing-whitespace t)
(setq-default javascript-indent-level 2)

(setq-default visible-bell "yes")

(setq grep-command "grep -rn ")
(setq grep-find-ignored-directories '(".hg" ".git"))
(setq c-basic-offset 4) ; Indent c code four spaces
(setq lua-indent-level 4) ; Indent lua code four spaces

(put 'upcase-region 'disabled nil)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
	(backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(require 'css-mode)
(require 'rst)
;; (require 'auto-complete-config)
(require 'dirtree)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(setq auto-mode-alist
      (cons '("\\.md$" . markdown-mode) auto-mode-alist))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   '(company-bbdb company-semantic company-cmake company-capf company-clang
		  company-files
		  (company-capf company-dabbrev-code company-gtags
				company-etags company-keywords)
		  company-oddmuse company-dabbrev))
 '(package-selected-packages
   '(blacken company desktop dirtree flycheck flycheck-pyflakes git-gutter
	     go-autocomplete lua-mode magit markdown-mode neotree
	     outline-indent popup python-mode pyvenv rust-mode tabbar
	     xml-format yaml-mode))
 '(safe-local-variable-values '((encoding . utf8)))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t)
 '(user-mail-address "rob@nonsequitarian.org"))

(global-git-gutter-mode +1)

;; Associate various HTML templating languages w/ html-mode
(setq auto-mode-alist
      (cons '("\\.pt$" . html-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mako$" . html-mode) auto-mode-alist))

;; Associate .rst and .rest extensions w/ rst-mode
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode)) auto-mode-alist))

;; Load javascript mode and associate w/ js and json files
(autoload 'javascript-mode "javascript" nil t)
(setq auto-mode-alist
      (cons '("\\.js$" . javascript-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.json$" . javascript-mode) auto-mode-alist))

(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

(set-default 'cursor-type 'box)
(setq default-frame-alist '((cursor-color . "white")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key "\C-xp" (lambda ()
			  (interactive)
			  (other-window -1)))
