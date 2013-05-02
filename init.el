;;;-----------------------------------------------------------------------------
;;; Todo items
;;;-----------------------------------------------------------------------------

;; * Use argv to load or skip loading certain parts of the config.
;;   This way, I can add a config suitable for commit messages.
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
;; Package init not needed, since it is done anyway in emacs 24 after reading the init
;; but we have to load the list of available packages
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
(package-require 'paredit)
(package-require 'clojure-mode)
(package-require 'nrepl)
(package-require 'nrepl-ritz)

;; Make sure paredit is active when clojure mode is active.
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Pretty fn, from emacs starter kit (esk).
(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u03BB"
                                                           'decompose-region)))))))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)


;;;-----------------------------------------------------------------------------
;;; Auto-completion
;;;-----------------------------------------------------------------------------

;; Have fuzzy completion, but does not seem to work yet...
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
                          lisp-mode latex-mode))
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
  (print "untabify?")
  (when (not indent-tabs-mode)
    (print "untabify!")
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-maybe)


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

;; Have multiple cursors, based on the current region. Use C-d to have a cursor
;; on the next occurence. The package advices to use C-> and C-<, but that I
;; did not get that to wirk with osx terminals (Terminal.app and iTerm2). C-d
;; comes close to the Apple-d I was used to with SublimeText 2.
(package-require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

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
