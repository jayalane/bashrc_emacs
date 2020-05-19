
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(sleep-for 1)
(package-initialize)
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl" "--control-stack-size 1000") :coding-system utf-8-unix)))
(setq slime-contribs '(slime-fancy))

(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8


(push "~/.emacs.d/lisp" load-path)

(require 'hcl)
(require 'terraform)

;;(add-hook 'terraform-mode-hook
;;		  (function (lambda ()
;;					  (add-hook (make-local-variable 'after-save-hook)
;;                              (function (lambda (
;; note:  the golang package has a good example here but it's complicated

(server-start)

(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-xm" 'compile)


(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
	    '(
	      (background-color . "black")
	      (cursor-color . "purple")
	      (foreground-color . "green"))

	    )))
  
(setq default-frame-alist initial-frame-alist)
  
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/Users/c60932a/bin/pycheckers.sh"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(if (< 25 emacs-major-version)
;    (progn (
;	    (package-initialize)
;)))

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(compile-command "go fmt ; golint ; go build ; go test")
 '(package-selected-packages (quote (memory-usage))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
