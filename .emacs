;;; package: dot emacs  -*- lexical-binding: t; -*-
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

 ;; Install the grammar first (one-time)
 ;;  (setq treesit-language-source-alist
 ;;        '((scala "https://github.com/tree-sitter/tree-sitter-scala")))
 ;; Then run: M-x treesit-install-language-grammar RET scala RET

;; Use scala-ts-mode
(use-package scala-ts-mode
    :mode "\\.scala\\'"
    :hook (scala-ts-mode . lsp-deferred))

;; gnus / email

;; Set your email identity
(setq user-mail-address "chris@lane-jayasinha.com"
        user-full-name "Chris Lane")

;; Always generate a From header using user-full-name and user-mail-address
(setq message-generate-headers-first '(From)
      message-default-mail-headers (format "From: %s <%s>\n" user-full-name user-mail-address)
      mail-host-address "lane-jayasinha.com")

;; Also add From header for M-x mail (mail-mode)
(setq mail-default-headers (format "From: %s <%s>\n" user-full-name user-mail-address))

;; Use IMAP as primary mail source
(setq gnus-select-method
        '(nnimap "chris@lane-jayasinha.com"
                 (nnimap-address "mail.lane-jayasinha.com")
                 (nnimap-server-port 993)
                 (gnus-search-engine gnus-search-imap)
                 (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
        '((nnimap "Jayalane"
                  (nnimap-address "mail.disputingtaste.com")
                  (nnimap-server-port 993)
                  (gnus-search-engine gnus-search-imap)
                  (nnimap-user "jayalane")
                  (nnimap-stream ssl))
          (nnimap "Clanstin"
                  (nnimap-address "mail.disputingtaste.com")
                  (nnimap-server-port 993)
                  (gnus-search-engine gnus-search-imap)
                  (nnimap-user "clanstin")
                  (nnimap-stream ssl))))

;; Enter groups with all articles visible at first
(setq gnus-fetch-old-headers nil)

;; Auto-expiry: articles marked 'E' get deleted after this many days
;; Use 'immediate to delete right away, or a number for days to keep
(setq nnmail-expiry-wait 'immediate)

;; Treat 'd' (mark read) as expirable in mail groups
(setq gnus-auto-expirable-newsgroups ".*")

;; CRITICAL: Actually expunge deleted messages from IMAP server
(setq nnimap-expunge 'on-exit)  ; Expunge when leaving group
;; Alternatives: 'immediately (expunge right away) or 'never (manual only)

;; For sending mail via SMTP (default: lane-jayasinha.com)
(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mail.lane-jayasinha.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

;; Set From address based on which IMAP group we're in
(setq gnus-posting-styles
      '(("nnimap\\+chris@lane-jayasinha\\.com:.*"
         (address "chris@lane-jayasinha.com"))
        ("nnimap\\+chris@disputingtaste\\.com:.*"
         (address "chris@disputingtaste.com"))))

;; Switch SMTP server based on From address when sending
(defun my-set-smtp-server ()
  "Set SMTP server based on the From header."
  (let ((from (message-fetch-field "From")))
    (if (and from (string-match "disputingtaste\\.com" from))
        (progn
          (setq smtpmail-smtp-server "mail.disputingtaste.com"
                smtpmail-smtp-service 587
                smtpmail-stream-type 'starttls))
      (setq smtpmail-smtp-server "mail.lane-jayasinha.com"
            smtpmail-smtp-service 587
            smtpmail-stream-type 'starttls))))

(add-hook 'message-send-hook 'my-set-smtp-server)
;; GPG encryption for authinfo
(require 'epa-file)
(epa-file-enable)  
(setq auth-sources '("~/.authinfo.gpg"))

;; sbcl
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl" "--control-stack-size 1000") :coding-system utf-8-unix)))
(setq slime-contribs '(slime-fancy))
(cond ((or (boundp 'window-system) (window-system)) (message "window system"))
      t (message "No window system"))

(require 'scala-repl)


(setq initial-major-mode 'emacs-lisp-mode)
(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8
(setf dired-kill-when-opening-new-dired-buffer t)

(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/s3ed" load-path)
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

;; make ansi term declare xterm-color - getting weird artifacts with eterm-color
(setq term-term-name "xterm-color")

(setq auto-save-default t)
(setq auto_save-interval 5)
(setq auto_save-timeout 5)
(setq mail-from-style 'system-default)
(setq scroll-step 1)
(setq comint-input-ring-size 10000000)
(setq comint-buffer-maximum-size 500000)

(setq mastodon-active-user "jayalane")
(setq mastodon-instance-url "https://mastodon.online")

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
  kept-old-versions 10    ; and how many of the old
  )

(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Set frame colors - works for both regular Emacs and daemon/emacsclient
(setq default-frame-alist
      '((background-color . "black")
        (cursor-color . "purple")
        (foreground-color . "green")
        (top . 50)
        (left . 300)
        (width . (text-pixels . 1200))
        (height . (text-pixels . 800))))

;; Settings for the initial frame
(setq initial-frame-alist
      '((background-color . "black")
        (cursor-color . "purple")
        (foreground-color . "green")
        (top . 50)
        (left . 300)
        (width . (text-pixels . 1200))
        (height . (text-pixels . 800))))

;; Also apply to frames created after init (for daemon mode)
;; Track frame count for diagonal cascading position
(defvar my-frame-counter 0
  "Counter for cascading frame positions along diagonal.")

(defun my-frame-setup (frame)
  "Apply color and geometry settings to new frames (for emacsclient).
Positions frames along a diagonal from (200,20) down-left in steps of 20px."
  (when (display-graphic-p frame)
    ;; Calculate diagonal position (wraps after 10 frames)
    (let* ((step (* (mod my-frame-counter 10) 20))
           (left (- 200 step))
           (top (+ 20 step)))  ; Start at 20 to clear menu bar
      ;; Set all frame parameters at once
      (modify-frame-parameters frame
                               `((background-color . "black")
                                 (foreground-color . "green")
                                 (cursor-color . "purple")
                                 (left . ,left)
                                 (top . ,top)
                                 (width . (text-pixels . 1200))
                                 (height . (text-pixels . 800)))))
    (setq my-frame-counter (1+ my-frame-counter))))

(add-hook 'after-make-frame-functions #'my-frame-setup)

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
  
;; (setq default-frame-alist initial-frame-alist)
  
(autoload 'calculator "calculator"
     "Run the Emacs calculator." t)

(require 'flymake)
(require 'flymake-golangci)
(require 'flymake-python-pyflakes)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/jayalane/bin/pycheckers.sh"  (list local-file)))))
;;  (add-to-list 'flymake-allowed-file-name-masks
;;               '("\\.py\\'" flymake-pyflakes-init)))


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
 '(connection-local-criteria-alist
   '(((:application vc-git) vc-git-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((vc-git-connection-default-profile (vc-git--program-version))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
                         "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
                         "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . tramp-ps-time)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (ttname . string)
                                          (time . tramp-ps-time)
                                          (nice . number)
                                          (etime . tramp-ps-time)
                                          (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string)
                                          (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . number)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))))
 '(package-selected-packages
   '(protobuf-mode company-go company w3m disk-usage lsp-mode google-maps
                   markdown-mode gptel yaml flycheck-yamllint
                   go-fill-struct go-direx go-errcheck go-stacktracer
                   go-rename go-complete protobuf-mode ox-epub ess
                   go-mode go-guru go-autocomplete go golint
                   golden-ratio mines magit memory-usage go-guru
                   matlab-mode magit nov latex-preview-pane
                   latex-math-preview latex-extra lean-mode
                   flycheck-golangci-lint lsp-latex tree-sitter
                   go-stacktracer go-complete go-autocomplete
                   go-expr-completion go-gopath go-dlv ess sudoku
                   slime memory-usage))
 '(send-mail-function 'mailclient-send-it))
   ;;'(transient claude-code protobuf-mode company-go company disk-usage lsp-mode google-maps markdown-mode gptel yaml flycheck-yamllint go-fill-struct go-direx go-errcheck go-stacktracer go-rename go-complete protobuf-mode ox-epub ess go-mode go-guru go-autocomplete go golint golden-ratio mines magit memory-usage go-guru matlab-mode magit nov latex-preview-pane latex-math-preview latex-extra lean-mode flycheck-golangci-lint lsp-latex tree-sitter go-stacktracer go-complete go-autocomplete go-expr-completion go-gopath go-dlv ess sudoku slime memory-usage)) 
;;   '(claude-code company company-go disk-usage ess ess
;;                 flycheck-golangci-lint flycheck-yamllint go
;;                 go-autocomplete go-autocomplete go-complete
;;                 go-complete go-direx go-dlv go-errcheck
;;                 go-expr-completion go-fill-struct go-gopath go-guru
;;                 go-guru go-mode go-rename go-stacktracer
;;                 go-stacktracer golden-ratio golint google-maps gptel
;;                 latex-extra latex-math-preview latex-preview-pane
;;                 lean-mode lsp-latex lsp-mode magit magit
;;                 markdown-mode matlab-mode memory-usage memory-usage
;;                 mines nov ox-epub protobuf-mode protobuf-mode slime
;;                 sudoku transient tree-sitter vterm w3m yaml)))
;; '(package-vc-selected-packages
;;  '((pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs"))))
;;  need system to merge these better - alpha and 1 per line I guess
;;   '(protobuf-ts-mode w3m rust-mode jq-ts-mode jq-mode mastodon vterm flycheck lsp-metals sbt-mode scala-repl scala-ts-mode minesweeper transient claude-code protobuf-mode company-go company w3m disk-usage lsp-mode google-maps markdown-mode gptel yaml flycheck-yamllint go-fill-struct go-direx go-errcheck go-stacktracer go-rename go-complete protobuf-mode ox-epub ess go-mode go-guru go-autocomplete go golint golden-ratio mines magit memory-usage go-guru matlab-mode magit nov latex-preview-pane latex-math-preview latex-extra lean-mode flycheck-golangci-lint lsp-latex tree-sitter go-stacktracer go-complete go-autocomplete go-expr-completion go-gopath go-dlv ess sudoku slime memory-usage))

;; '(send-mail-function 'sendmail-send-it))


(defun remove-entry (key lst)
  "Remove the association with KEY from LST. - by chatGPT"
  (delete (assoc key lst) lst))

(require 'auctex)
;; (setq LaTeX-indent-environment-list (remove-entry "align*" LaTeX-indent-environment-list))

(require 'disk-usage)

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
(setenv "SUDO_PROMPT" "[sudo] password for %u: ")
(setenv "GODEBUG" "x509sha1=1")
(defun comint-password-function-impl (a)
  "interact password"
  "bad-password") ;; set this to the desired password (simple simple function)

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
    vterm-mode-hook
    go-mode-hook))

;; Enable visual-line-mode and line numbers for specific modes
(dolist (mode my-visual-line-modes)
  (add-hook mode (lambda ()
                   (human-text-on))))

(defun magit-git-executable ()
  "/usr/bin/git")

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

(require 'pgmacs)
(require 'dash)
(require 's3ed)

(defun s3ed-load-aws-creds (&optional profile)
  "Load AWS credentials by running get_creds.sh.
PROFILE defaults to \"prod-ro\"."
  (interactive "sAWS profile (default prod-ro): ")
  (when (or (null profile) (string-empty-p profile))
    (setq profile "prod-ro"))
  (let ((output (shell-command-to-string
                 (format ". ~/bin/get_creds.sh %s ; set | grep ^AWS_"
                         (shell-quote-argument profile)))))
    (dolist (line (split-string output "\n" t))
      (when (string-match "^\\(AWS_[^=]+\\)=\\(.*\\)$" line)
        (setenv (match-string 1 line) (match-string 2 line))))
    (message "AWS credentials loaded for %s" profile)))

;;; Set search_path for pgmacs/pg.el connections
(add-to-list 'pg-new-connection-hook
             (lambda (con)
               (pg-exec con "SET search_path TO etl_control, common, reporting, client_fcc, client_twh;")))

;;; Redshift: pg-tables legacy path doesn't work; override to use information_schema
(defun my-pg-tables-redshift (orig-fun con)
  "Use information_schema.tables for Redshift, which reports as PostgreSQL 8.x."
  (let* ((res (pg-exec con
               "SELECT DISTINCT table_schema, table_name FROM information_schema.tables
                WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
                AND table_type IN ('BASE TABLE', 'VIEW')"))
         (tuples (pg-result res :tuples)))
    (cl-loop for tuple in tuples
             collect (let ((schema (cl-first tuple))
                           (name (cl-second tuple)))
                       (make-pg-qualified-name :schema schema :name name)))))

(advice-add 'pg-tables :around #'my-pg-tables-redshift)

;;; Redshift connection
(setq sql-connection-alist
      '((redshift
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-port 5439)
         (sql-database "level")
         (sql-user "dbt"))))

(defun my-sql-redshift-hook ()
  "Set search_path after connecting to Redshift."
  (when (eq sql-product 'postgres)
    (sql-send-string "SET search_path TO etl_control, common, reporting, client_fcc, client_twh;")))

(add-hook 'sql-login-hook #'my-sql-redshift-hook)

;;; .emacs ends here

