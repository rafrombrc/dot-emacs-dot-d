(let ((default-directory "~/.emacs.d/site-lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
(setq-default transient-mark-mode t)
(setq-default auto-fill-mode t)
(setq-default show-trailing-whitespace t)

(global-set-key [(control \.)] 'goto-line)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-]" 'help-for-help)

;;(set-face-font 'default "fontset-standard")
;;(set-fontset-font "fontset-default"
;;		  'latin-iso8859-1
;;		  "-Misc-Fixed-Medium-R-Normal--14-130-75-75-C-70-ISO8859-1")
(setq default-frame-alist '((font . "7x14")))
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")
(set-face-background 'default' "black")
(set-face-foreground 'default' "white")
(set-fill-column 79)
(setq-default indent-tabs-mode nil)
(global-set-key [(control \.)] 'goto-line)

(setq grep-command "grep -rn ")

(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))

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
(require 'doctest-mode)
(require 'rst)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

; Associate .rst and .rest extensions w/ rst-mode
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))


; Indent c code four spaces
(setq c-basic-offset 4)
; Associate c-mode with the .js extension
; (setq auto-mode-alist (append '(("\\.js$" . c-mode)) auto-mode-alist))

;; Options Menu Settings
;; =====================
(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (or (and
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (= emacs-major-version 20))
       (fboundp 'load-options-file))
  (load-options-file "/home/rob/.xemacs-options")))
;; ============================
;; End of Options Menu Settings

;; load oo-browser
;; (setq load-path (append
;;                  '("/home/rob/src/oo-browser/"
;;                    "/home/rob/src/oo-browser/hypb/")
;;                  load-path))
;; (load "br-start")
;; (global-set-key "\C-c\C-o" 'oo-browser)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(user-mail-address "rob@kalistra.com"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 )

;; overlay-fix.el --- overlay bug workaround

;; Copyright (C) 2001 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 12 Feb 2001

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; This library advices the built-in functions `make-overlay' and
;; `overlay-put' to ensure that the overlay 'face property is never
;; nil but `overlay-empty-face' instead.  In Emacs versions before 21
;; when the overlay 'face property is nil it can disable the overlayed
;; text face property.  The most visible issue is that overlayed text
;; is not syntax highlighted.

;;; History:
;; 

;;; Code:
(if (or (featurep 'xemacs) (> emacs-major-version 20))
    ;; Not needed for Emacs since v21 and XEmacs
    nil

  ;; Define an empty face
  (make-empty-face 'overlay-empty-face)

  (defadvice make-overlay (after after-make-overlay activate)
    "Ensure overlay 'face property is never nil."
    (or (overlay-get ad-return-value 'face)
        (overlay-put ad-return-value 'face 'overlay-empty-face)))

  (defadvice overlay-put (before before-overlay-put activate)
    "Ensure overlay 'face property is never nil."
    (and (eq (ad-get-arg 1) 'face)
         (null (ad-get-arg 2))
         (ad-set-arg 2 'overlay-empty-face)))
  
  )

(provide 'overlay-fix)

;;; overlay-fix.el ends here

;; javascript-mode stuff
(autoload 'javascript-mode "javascript" nil t)
(setq auto-mode-alist
      (cons '("\\.js$" . javascript-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.json$" . javascript-mode) auto-mode-alist))

;;(require 'pycomplete)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(setq interpreter-mode-alist(cons '("python" . python-mode)
                             interpreter-mode-alist))

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

(setq auto-mode-alist
      (cons '("\\.dtml$" . html-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.pt$" . html-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.cpt$" . html-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.zcml$" . xml-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mako$" . html-mode) auto-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.cpy$" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.vpy$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
	    interpreter-mode-alist))


(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))


(put 'upcase-region 'disabled nil)


;; flymake pyflakes stuff
(when (load "flymake" t)
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                       'flymake-create-temp-inplace)) 
           (local-file (file-relative-name 
                        temp-file 
                        (file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file)))) 

  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))) 

(add-hook 'find-file-hook 'flymake-find-file-hook)


;; Additional functionality that makes flymake error messages appear
;; in the minibuffer when point is on a line containing a flymake
;; error. This saves having to mouse over the error, which is a
;; keyboard user's annoyance

;;flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (second elem))))
	    (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
	((null (flymake-ler-file err))
	 ;; normal message do your thing
	 (flymake-ler-text err))
	(t ;; could not compile err
	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook))) 
