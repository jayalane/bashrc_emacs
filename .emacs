;;; package: dot emacs
;;; Commentary: Chris Lane .emacs

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
(setf dired-kill-when-opening-new-dired-buffer t)

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

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

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

(require 'flymake)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/Users/chlane/bin/pycheckers.sh"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))


(add-hook 'find-file-hook 'flymake-find-file-hook)

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
 '(compile-command
   "make ; go mod init ; go mod tidy ; go fmt ; golint ; go vet ; go build  ; golangci-lint run . ; staticcheck ; go test -v -v -race -coverprofile fmtcoverage.html")
 '(package-selected-packages
   '(w3m disk-usage lsp-mode google-maps markdown-mode gptel yaml flycheck-yamllint go-fill-struct go-direx go-errcheck go-stacktracer go-rename go-complete protobuf-mode ox-epub ess go-mode go-guru go-autocomplete go golint golden-ratio mines magit memory-usage go-guru)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; (setenv "namespace" "test")
;; (setenv "namespace" "local")
(setenv "AZ" "local1")
(defun comint-password-function-impl (a)
  "interact password"
  "bad-password")

(setq comint-password-function 'comint-password-function-impl)

(comint-password-function-impl 'a)
(put 'scroll-left 'disabled nil)

;; chat GPT4 stuff

(defun find-count-api ()
  "Find and change 'count.Incr' to 'count.IncrSync'
   and 'count.IncrSuffix' to 'count.IncrSyncSuffix'
   in the current Go buffer."
  (interactive)
  (when (derived-mode-p 'go-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "count.Incr" nil t)
        (replace-match "count.IncrSync" nil t))
      (goto-char (point-min))
      (while (search-forward "count.IncrSuffix" nil t)
        (replace-match "count.IncrSyncSuffix" nil t)))
    (message "API replacements complete."))
  (unless (derived-mode-p 'go-mode)
    (message "This function should be called in a Go buffer.")))


(defun format-number-with-commas (number)
  "Format NUMBER with commas as thousand separators."
  (let* ((number-str (number-to-string number))
         (integer-part (substring number-str 0 (or (string-match "\\." number-str) (length number-str))))
         (fractional-part (if (string-match "\\." number-str) (substring number-str (match-beginning 0)) ""))
         (result ""))
    (dotimes (i (length integer-part) (concat result fractional-part))
      (setq result (concat (char-to-string (aref integer-part (- (length integer-part) (1+ i)))) result))
      (when (and (= (mod (1+ i) 3) 0) (< i (1- (length integer-part))))
        (setq result (concat "," result))))))

(defun insert-number-with-commas (number)
  "Insert NUMBER with commas as thousand separators on the next line in the current buffer."
  (let ((formatted-number (format-number-with-commas number)))
    (end-of-line)
    (insert "\n" formatted-number)))
