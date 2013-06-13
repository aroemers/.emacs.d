;;;-----------------------------------------------------------------------------
;;; Todo items
;;;-----------------------------------------------------------------------------

;; * Automatically open a new comment line when pressing RET while on a comment
;;   line.


;;;-----------------------------------------------------------------------------
;;; Convenient package handling in emacs
;;;-----------------------------------------------------------------------------

(require 'package)

;; Add automatically parsed versiontracking repositories.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Make sure a package is installed
(defun package-require (package)
  "Install a package unless it is already installed
or a feature with the same name is already active.

Usage: (package-require 'package)"
  ; try to activate the package with at least version 0.
  (package-activate package '(0))
  ; try to just require the package. Maybe the user has it in his local config
  (condition-case nil
      (require package)
    ; if we cannot require it, it does not exist, yet. So install it.
    (error (when (not (package-installed-p package))
             (package-install package)))))

;; Initialize installed packages
(package-initialize)
;; Package init not needed, since it is done anyway in emacs 24 after reading
;; the init but we have to load the list of available packages
(when (not package-archive-contents)
  (package-refresh-contents))


;;;-----------------------------------------------------------------------------
;;; Theming
;;;-----------------------------------------------------------------------------

;; Load the solarized package and activate it.
(package-require 'color-theme-solarized)
(load-theme 'solarized-dark t)

;; Set the color of the modeline (statusbar).
(set-face-background 'mode-line "#ddddee") ; the text color.
(set-face-foreground 'mode-line "#337744") ; the bar color.

;; Disable the menubar, toolbar, scrollbars and set a decent size for the window
;; when using the window system.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when window-system
  (set-frame-size (selected-frame) 180 60))


;;;-----------------------------------------------------------------------------
;;; Clojure and nrepl modes
;;;-----------------------------------------------------------------------------

;; Require the needed packages.
(package-require 'clojure-mode)
(package-require 'nrepl)
(package-require 'nrepl-ritz)

;; Make sure paredit is active when clojure mode is active.
(package-require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Pretty fn, from emacs starter kit (esk).
(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u03BB"
                                                           'decompose-region)))))))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)

;; Add better indentation for some symbols.
(define-clojure-indent
  (try-let 1)
  (with-resource 'defun)
  (fact 1))


;;;-----------------------------------------------------------------------------
;;; Scala mode
;;;-----------------------------------------------------------------------------

;; Require the needed packages and make sure it is active when a Scala file is
;; loaded. Note that there is also a Scala-mode2, which can check out some day.
(package-require 'scala-mode)
(require 'scala-mode-auto)


;;;-----------------------------------------------------------------------------
;;; Auto-completion
;;;-----------------------------------------------------------------------------

;; Have fuzzy completion, but does not seem to work yet as expected.
(package-require 'fuzzy)

;; Have auto-completion, with standard settings..
(package-require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; Use the following sources for auto-completion.
(set-default 'ac-sources
             '(ac-source-dictionary                  ; from dict files.
               ac-source-words-in-buffer             ; current buffer
               ac-source-words-in-same-mode-buffers  ; alike buffers
               ac-source-filename                    ; filesystem paths
               ac-source-functions                   ; elisp functions
               ac-source-symbols))                   ; elisp symbols

;; Use auto-completion in the following modes.
(dolist (mode '(text-mode html-mode nxml-mode sh-mode clojure-mode
                          lisp-mode latex-mode scala-mode))
  (add-to-list 'ac-modes mode))

;; Map auto-complete to M-TAB, to use if it does not pop up automatically.
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; Have auto-completion using the nrepl session.
(package-require 'ac-nrepl)


;;;-----------------------------------------------------------------------------
;;; Tab and spaces handling
;;;-----------------------------------------------------------------------------

;; Make Emacs ask about missing newline at end of file on save.
(setq require-final-newline 'ask)

;; Remove trailing spaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use no tabs by default. Modes that really need tabs should enable
;; indent-tabs-mode explicitly. And if indent-tabs-mode is off, untabify
;; before saving.
(setq-default indent-tabs-mode nil)

(defun untabify-maybe ()
  (when (not indent-tabs-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-maybe)


;;;-----------------------------------------------------------------------------
;;; IRC related
;;;-----------------------------------------------------------------------------

;; Ignore joins and leaves in IRC.
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; Scroll down to the bottom automatically.
(add-hook 'erc-mode-hook 'erc-scrolltobottom-mode)

;; Auto-connect when a password is given.
(defun irc-maybe ()
  "Connect to IRC, when a password is given."
  (let ((p (read-passwd "Give password to connect to IRC: ")))
    (when (> (length p) 0)
      (erc-tls :server "ie.freebnc.net" :port 6697
               :nick "aroemers" :password p))))

;; Ask to auto-connect on startup.
(add-hook 'emacs-startup-hook 'irc-maybe)

;; Have colorised nick-names.
(package-require 'erc-hl-nicks)


;;;-----------------------------------------------------------------------------
;;; Other niceties
;;;-----------------------------------------------------------------------------

;; Add my own write mode.
(load-file "~/.emacs.d/write-mode.el")

;; Browse through the undo tree, using C-x u
(package-require 'undo-tree)
(global-undo-tree-mode)

;; Show what has changed since the last commit in a file.
(package-require 'git-gutter)
(global-git-gutter-mode t)

;; Easily expand a region to the enclosing scope, using backtab (shift+tab).
;; The package advices C-= as key, but it is difficult to get that to work with
;; osx terminals (Terminal.app and iTerm2). Haven't fould a solution yet.
;; This package is Clojure compatible.
(package-require 'expand-region)
(global-set-key [backtab] 'er/expand-region)

;; Have multiple cursors, based on the current region (or just a cursor on the
;; next line if no region is selected). Use M-n and/or M-p to have a cursor
;; on the next or previous occurence. The package advices to use C-> and C-<,
;; but this won't work with osx terminals (Terminal.app and iTerm2) without
;; extensive abuse of keycodes. Also, M-x mc/mark-more-like-this-extended RET
;; is helpful when quickly adding and skipping occurences.
(package-require 'multiple-cursors)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)

;; Have ido like completions for M-x (execute-extended-command).
;; It also gives a M-X shortcut to only show the commands from the
;; currend major mode.
(package-require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Have a small amount of time to press o after C-x o to switch quickly to the
;; next window. Only active when having 3 or more windows.
(package-require 'win-switch)
(global-set-key (kbd "C-x o") 'win-switch-dispatch)
(global-set-key (kbd "M-o") 'win-switch-dispatch)

;; Go straight to the *scratch* buffer, i.e. skip the help message. And set a
;; nice welcoming message.
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking, Arnout!\n\n")

;; Show column number next to linenumber in the status bar.
(column-number-mode t)

;; Enable ido mode. Remember that C-j uses the current selection in the ido
;; minibuffer, which comes in handy when selecting a dired directory (M-x dired).
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Wrap words instread of breaking them.
(global-visual-line-mode t)

;; Show matching paren.
(show-paren-mode t)

;; Go to the last change, using M-.
;; M-. can conflict with etags, but I don't use that feature.
(package-require 'goto-chg)
(global-set-key (kbd "M-.") 'goto-last-change)

;; Save backups and autosaves in the system's temporary directory.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Hilight the current line.
(global-hl-line-mode t)

;; Self-inflicted masochism.
;(package-require 'guru-mode)
;(guru-global-mode t)

;; Switch back to previous buffer, with C-c b.
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; Don't require two spaces between sentences when moving with M-e and M-a.
(setq sentence-end-double-space nil)

;; Add projectile, e.g. for quickly opening files and searching in files.
(package-require 'projectile)
(projectile-global-mode t)

;; Integrate with the system's clipboard, does not seem to work in the
;; terminal on OS X Mountain Lion, while it does work in a terminal on
;; OS X Lion.
(package-require 'simpleclip)
(simpleclip-mode t)

;; Automatic indention on RET and yanks, and only one space on forward-kills.
(package-require 'auto-indent-mode)
(auto-indent-global-mode t)
(add-to-list 'auto-indent-disabled-modes-list 'nrepl-mode)
(setq auto-indent-blank-lines-on-move nil) ; No indent while moving, it's weird

;; Have some smoother scrolling when on a window system.
(when window-system
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))

;; Have smoother scrolling with keyboard.
(setq scroll-margin 2
      scroll-conservatively 1000)

;; Use git from within emacs.
(package-require 'magit)

;; Have emacs reload buffers from the disk when changed. Currently, it does not
;; warn when the buffer is modified _and_ the the file on disk has been
;; modified. Also, watch out for lots of traffic when opening files via a
;; network.
(global-auto-revert-mode t)

;; Have the home and end key behave as they should within emacs, i.e. move to
;; the beginning or end of the line, respectively.
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

;; Have Paredit enabled while in emacs-lisp mode.
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Open the current file as root.
(defun current-as-root ()
  "Reopen current file as root"
  (interactive)
  (set-visited-file-name (concat "/sudo::" (buffer-file-name)))
  (setq buffer-read-only nil))

;; Disable the command key on OS X. This does imply that simpleclip functions
;; need to be called with M-x. Unfortunately, this also means Command-V (paste)
;; does not work either when in a window-system.
;(when (eq system-type 'darwin)
;  (setq mac-command-modifier nil))

;; Have a key for loading the init file quickly.
(global-set-key (kbd "C-c i")
                (lambda () (interactive)
                  (find-file-other-window "~/.emacs.d/init.el")))


;;;-----------------------------------------------------------------------------
;;; Emacs automagically managed settings. Don't touch :)
;;;-----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#262626" "#d70000" "#5f8700" "#af8700" "#0087ff" "#af005f" "#00afaf" "#626262"])
 '(background-color "#1c1c1c")
 '(background-mode dark)
 '(cursor-color "#808080")
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#808080"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
