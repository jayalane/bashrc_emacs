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
(setq org-todo-keywords '((type "TODO" "WONT" "DONE")))
(message "loading .emacs")

(sleep-for 1)
(package-initialize)
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl" "--control-stack-size 1000") :coding-system utf-8-unix)))
(setq slime-contribs '(slime-fancy))
(cond ((or (boundp 'window-system) (window-system)) (message "window system"))
      t (message "No window system"))

(setq initial-major-mode 'emacs-lisp-mode)
(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8
(setf dired-kill-when-opening-new-dired-buffer t)

(push "~/.emacs.d/lisp" load-path)
(setq vc-make-backup-files t)
(setq version-control t)
(add-to-list 'backup-directory-alist '("." . ".~"))
(setq kept-old-versions 10)
(setq kept-new-versions 10)

(setq auto-save-default t)
(setq auto_save-interval 5)
(setq auto_save-timeout 5)
(setq mail-from-style 'system-default)
(setq scroll-step 1)
(setq comint-input-ring-size 10000000)
(setq comint-buffer-maximum-size 500000)

(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq auto-save-default t)
(setq auto_save-interval 5)
(setq auto_save-timeout 5)
(setq mail-from-style 'system-default)
(setq scroll-step 1)
(setq comint-input-ring-size 10000000)
(setq comint-buffer-maximum-size 500000)

(setq mastodon-active-user "jayalane")
(setq mastodon-instance-url "https://mastodon.online")

(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'server)

(add-hook 'after-init-hook
          (lambda ()
            (unless (server-running-p)
              (server-start))))

(setq load-path (cons "~/emacs" load-path))
(setq load-path (cons "~/emacs/tnt" load-path))
(setq load-path (cons "~/emacs/pcl-cvs" load-path))
(setq load-path (cons "~/emacs/slime" load-path))
(setq load-path (cons "~/emacs/erc-cvs" load-path))
;(setq load-path (cons "~/emacs/tramp" load-path))
;(setq load-path (cons "~/emacs/ess/lisp" load-path))

(setq load-path (cons "~/emacs" load-path))
(setq load-path (cons "~/emacs/tnt" load-path))
(setq load-path (cons "~/emacs/pcl-cvs" load-path))
(setq load-path (cons "~/emacs/slime" load-path))
(setq load-path (cons "~/emacs/erc-cvs" load-path))
;(setq load-path (cons "~/emacs/tramp" load-path))
;(setq load-path (cons "~/emacs/ess/lisp" load-path))

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
      (setq default-frame-alist
	    '(
	      (background-color . "black")
	      (cursor-color . "purple")
	      (foreground-color . "green"))
		)))

(require 'sudoku)
(require 'url)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.fxml$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sdl$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.oml$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.mdf$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.py" . python-mode))

(require 'show-wspace)

(defmacro try-this (&rest body)
  `(unwind-protect
       (let (retval (gensym))
         (condition-case ex
             (setq retval (progn ,@body))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)))

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-verbose 10)
(setq tramp-debug-bufer t)

(setq tcl-default-application "tclsh")
(setq blink-matching-paren-distance 24000)

(require 'slime)
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(slime-setup)
  
(setq default-frame-alist initial-frame-alist)
  
(autoload 'calculator "calculator"
     "Run the Emacs calculator." t)

(require 'flymake)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/jayalane/bin/pycheckers.sh"  (list local-file))))
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
   "cd ~/go/src/github.paypal.com/chlane/rlcountserv/ ; namespace=test make")
 '(package-selected-packages
   '(protobuf-mode company-go company w3m disk-usage lsp-mode google-maps markdown-mode gptel yaml flycheck-yamllint go-fill-struct go-direx go-errcheck go-stacktracer go-rename go-complete protobuf-mode ox-epub ess go-mode go-guru go-autocomplete go golint golden-ratio mines magit memory-usage go-guru matlab-mode magit nov latex-preview-pane latex-math-preview latex-extra lean-mode flycheck-golangci-lint lsp-latex tree-sitter go-stacktracer go-complete go-autocomplete go-expr-completion go-gopath go-dlv ess sudoku slime memory-usage)))


(defun remove-entry (key lst)
  "Remove the association with KEY from LST. - by chatGPT"
  (delete (assoc key lst) lst))

(require 'latex)
(setq LaTeX-indent-environment-list (remove-entry "align*" LaTeX-indent-environment-list))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

(setq scheme-program-name "scheme")
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

(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(display-time)
(setq-default ispell-program-name "aspell")
;;(defun start-bigr ()
;; "Start R for ESS with big parameters"
;; (R "--min-vsize=500M --min-nsize=12M")
;;)
;; mail-crypt lines (pgp integration)
;(autoload 'mc-install-write-mode "mailcrypt" nil t)
;(autoload 'mc-install-read-mode "mailcrypt" nil t)
;(add-hook 'mail-mode-hook 'mc-install-write-mode)
;(add-hook 'rmail-mode-hook 'mc-install-read-mode)
;(add-hook 'rmail-summary-mode-hook 'mc-install-read-mode)
;(add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
;(add-hook 'news-reply-mode-hook 'mc-install-write-mode)
(set-exec-path-from-shell-PATH)

;; (setenv "namespace" "test")
;; (setenv "namespace" "local")
(setenv "AZ" "ccg01")
(setenv "GODEBUG" "x509sha1=1")
(defun comint-password-function-impl (a)
  "interact password"
  "bad-password")

(setq comint-password-function 'comint-password-function-impl)

(comint-password-function-impl 'a)
(put 'scroll-left 'disabled nil)

(defun human-text-on ()
  "Turn on human text options."
  (turn-on-visual-line-mode)
  (display-line-numbers-mode)
  )

(defvar my-visual-line-modes
  '(text-mode-hook
    prog-mode-hook
    comint-mode-hook
    shell-mode-hook
    eshell-mode-hook
    go-mode-hook))

;; Enable visual-line-mode and line numbers for specific modes
(dolist (mode my-visual-line-modes)
  (add-hook mode (lambda ()
                   (human-text-on))))

(defun magit-git-executable ()
	   return "/usr/bin/git")

(defun mapply (func args)
  (dolist (someargs args)
    (apply func someargs)))
;; chat GPT4 stuff
(defun mapply (func args)
  (dolist (someargs args)
    (apply func someargs)))

(mapply 'add-hook
        '((python-mode-hook ws-highlight-tabs)
          (python-mode-hook
           (lambda () (if (not (null buffer-file-name)) (flymake-mode))))))

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
    (interactive)
    (end-of-line)
    (insert "\n" formatted-number)))


(setq lsp-go-use-gofumpt t)

;;; .emacs ends here

