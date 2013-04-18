;;;-----------------------------------------------------------------------------
;;; Todo items
;;;-----------------------------------------------------------------------------

;; * Use argv to load or skip loading certain parts of the config. 
;;   This way, I can add a config suitable for commit messages.
;; * Improve the narrow-margins, make it a mode that scales with windows 
;;   resizing.
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
(set-face-background 'modeline "#ddddee") ; the text color.
(set-face-foreground 'modeline "#337744") ; the bar color.

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

;; Go straight to the *scratch* buffer, i.e. skip the help message. And set a
;; nice welcoming message.
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking, Arnout!\n\n")

;; Show column number next to linenumber in the status bar.
(column-number-mode t)

;; Enable ido mode.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Wrap words instread of breaking them.
(global-visual-line-mode t)


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
