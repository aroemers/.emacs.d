;;; arnouts-init --- Arnout's custom Emacs script.
;;;
;;; Commentary:
;;;   This is my custom Emacs init script. Currently it is mostly setup for
;;;   for Clojure and Scala development.
;;;
;;; Code:
;;;   The code is split up in logical sections.


;;;-----------------------------------------------------------------------------
;;; Root initialization.
;;;-----------------------------------------------------------------------------

(setq warning-minimum-level :error)


;;;-----------------------------------------------------------------------------
;;; Package handling.
;;;-----------------------------------------------------------------------------

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)


;;;-----------------------------------------------------------------------------
;;; Execution path.
;;;-----------------------------------------------------------------------------

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "JAVA_HOME")))


;;;-----------------------------------------------------------------------------
;;; Theming.
;;;-----------------------------------------------------------------------------

;; Load theme package and configure window.
(use-package dracula-theme
  :if
  window-system

  :config
  ;; Disable scrollbar and toolbar.
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (load-theme 'dracula t)

  ;; Update fonts.
  (set-face-attribute 'default nil :height 130)
  (setq-default line-spacing 3)

  ;; Set the color of some other parts of Emacs.
  (set-face-background 'cursor "chartreuse1")

  ;; Maximize frame on startup.
  (toggle-frame-maximized)

  :custom
  (mouse-wheel-scroll-amount '(2 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil))

;; Disable menu bar.
(menu-bar-mode -1)


;;;-----------------------------------------------------------------------------
;;; Clojure development.
;;;-----------------------------------------------------------------------------

;; Require the Clojure mode.
(use-package clojure-mode
  :config
  ;; Add better indentation for some forms.
  (define-clojure-indent
    (defstate 1)))

;; Require the Cider package for a REPL.
(use-package cider)

;; Enable paredit.
(use-package paredit
  :hook
  (clojure-mode . paredit-mode))

;; Completions in Cider.
(use-package company
  :hook
  (cider-repl-mode . company-mode)
  (cider-mode . company-mode)

  :bind
  ("M-SPC" . company-complete))

;; Use Clojure-lsp for static analysis.
(use-package lsp-mode
  :ensure t

  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))

  :config
  (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

;; For lsp UI-feedback.
(use-package lsp-ui
  :commands lsp-ui-mode

  :config
  (setq lsp-ui-doc-enable nil))

;; For lsp completions.
(use-package company-lsp
  :commands company-lsp)

;; Preview function definition arguments.
(use-package yasnippet
  :hook
  (clojure-mode . yas-minor-mode))


;;;-----------------------------------------------------------------------------
;;; Scala development.
;;;-----------------------------------------------------------------------------

;; (use-package scala-mode
;;   :mode
;;   "\\.s\\(cala\\|bt\\)$")

;; (use-package sbt-mode
;;   :commands
;;   sbt-start
;;   sbt-command

;;   :custom
;;   (sbt:program-options '("-Dsbt.supershell=false")))

;; (use-package flycheck
;;   :hook
;;   (scala-mode . flycheck-mode))

;; (use-package lsp-mode
;;   :hook
;;   (scala-mode . lsp)
;;   (lsp-mode . lsp-lens-mode)

;;   :custom
;;   (lsp-prefer-flymake nil))

;; (use-package lsp-metals)

;; (use-package lsp-ui)

;; (use-package yasnippet
;;   :hook
;;   (scala-mode . yas-minor-mode))

;; (use-package company
;;   :hook
;;   (scala-mode . company-mode)

;;   :bind
;;   ("M-SPC" . company-complete))

;; (use-package company-lsp)

;; (use-package lsp-treemacs
;;   :config
;;   (lsp-metals-treeview-enable t)

;;   :custom
;;   (lsp-metals-treeview-show-when-views-received t))

;;;-----------------------------------------------------------------------------
;;; Emacs Lisp development.
;;;-----------------------------------------------------------------------------

(use-package paredit
  :hook
  (emacs-lisp-mode . paredit-mode))

(use-package eldoc
  :hook
  (emacs-lisp-mode . eldoc-mode))

;; (use-package flycheck
;;   :hook
;;   (emacs-lisp-mode . flycheck-mode))


;;;-----------------------------------------------------------------------------
;;; Ido Mode related
;;;-----------------------------------------------------------------------------

;; Enable ido mode. Remember that C-j uses the current selection in
;; the ido minibuffer, which comes in handy when selecting a dired
;; directory (M-x dired).
(use-package ido
  :config
  (ido-mode t)

  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t))

;; Have a better ido-everywhere.
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode t))

;; Align the options vertically, and make up-down for options, and
;; left-right for history.
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)

  :custom
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

;; Have a better fuzzy matching, and highlight the matching characters.
(use-package flx-ido
  :config
  (flx-ido-mode 1))

;; Have ido like completions for M-x (execute-extended-command).
;; It also gives a M-X shortcut to only show the commands from the
;; currend major mode.
(use-package smex
  :config
  (smex-initialize)

  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))


;;;-----------------------------------------------------------------------------
;;; Common packages.
;;;-----------------------------------------------------------------------------

;; Browse through the undo tree, using C-x u
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Show what has changed since the last commit in a file.
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;; Easily expand a region to the enclosing scope, using backtab (shift+tab).
;; The package advices C-= as key, but it is difficult to get that to work with
;; osx terminals (Terminal.app and iTerm2). Haven't fould a solution yet.
;; This package is Clojure compatible.
(use-package expand-region
  :bind
  ("<backtab>" . er/expand-region))

;; Have multiple cursors, based on the current region (or just a cursor on the
;; next line if no region is selected). Use M-n and/or M-p to have a cursor
;; on the next or previous occurence. The package advices to use C-> and C-<,
;; but this won't work with osx terminals (Terminal.app and iTerm2) without
;; extensive abuse of keycodes. Also, M-x mc/mark-more-like-this-extended RET
;; is helpful when quickly adding and skipping occurences.
(use-package multiple-cursors
  :bind
  ("M-p" . mc/mark-previous-like-this)
  ("M-n" . mc/mark-next-like-this)
  ("M-a" . mc/mark-all-like-this-dwim))

;; Go to the last change, using M-l.
(use-package goto-last-change
  :bind
  ("M-l" . goto-last-change))

;; Add projectile, e.g. for quickly opening files and searching in files.
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)

  :config
  (projectile-mode t)

  :custom
  ;; Make rgrep work.
  (shell-file-name "/bin/sh")
  (projectile-use-git-grep t))


;; Automatic indention on RET and yanks, and only one space on forward-kills.
(use-package auto-indent-mode
  :config
  (auto-indent-global-mode t)
  (add-to-list 'auto-indent-disabled-modes-list 'cider-mode)

  :custom
  ;; No indenting while moving, it's weird.
  (auto-indent-blank-lines-on-move nil))

;; Use git from within emacs.
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; Have too long lines highlighted.
(use-package whitespace
  :hook
  (prog-mode . whitespace-mode)

  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face lines-tail)))

;; Have window numbers for faster switching.
(use-package window-numbering
  :config
  (window-numbering-mode t))

;; Add a markdown mode
(use-package markdown-mode)

;; Add nyan cat.
(use-package nyan-mode
  :if
  window-system

  :config
  (nyan-mode 1)

  :custom
  (nyan-minimum-window-width 175))

;; Install yaml-mode
(use-package yaml-mode)

;; Display bound keys
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

;; Higlight todo, fixme, etc
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Add restclient
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

;; Support mustache templating
(use-package mustache-mode
  :ensure t)


;;;-----------------------------------------------------------------------------
;;; Tab and spaces handling.
;;;-----------------------------------------------------------------------------

;; Make Emacs ask about missing newline at end of file on save.
(setq require-final-newline 'ask)

;; Remove trailing spaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use no tabs by default. Modes that really need tabs should enable
;; indent-tabs-mode explicitly. And if indent-tabs-mode is off, untabify
;; before saving.
(setq-default indent-tabs-mode nil)


;;;-----------------------------------------------------------------------------
;;; Term related settings.
;;;-----------------------------------------------------------------------------

;; Clear ansi-term buffer with C-c C-b (Clear Buffer).
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-c C-b")
              (lambda ()
                (interactive)
                (term-reset-terminal)))))

;; Have UTF-8 encoding in terminals.
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; Set default shell binary.
(setq explicit-shell-file-name "/bin/zsh")


;;;-----------------------------------------------------------------------------
;;; Other niceties.
;;;-----------------------------------------------------------------------------

;; Go straight to the *scratch* buffer, i.e. skip the help message.
;; And set a nice welcoming message.
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking, Arnout!\n\n")
(setq initial-major-mode 'clojure-mode)

;; Show column number next to linenumber in the status bar.
(column-number-mode t)

;; Wrap words instread of breaking them.
(toggle-word-wrap t)

;; Show matching paren.
(show-paren-mode t)

;; Save backups and autosaves in the system's temporary directory.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Switch back to previous buffer, with C-c b.
(defun switch-to-previous-buffer ()
  "Switch to the previously active buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) nil)))
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; Don't require two spaces between sentences when moving with M-e and M-a.
(setq sentence-end-double-space nil)

;; Have smoother scrolling with keyboard.
(setq scroll-margin 2
      scroll-conservatively 1000)

;; Have emacs reload buffers from the disk when changed. Currently, it does not
;; warn when the buffer is modified _and_ the the file on disk has been
;; modified. Also, watch out for lots of traffic when opening files via a
;; network.
(global-auto-revert-mode t)

;; Have the home and end key behave as they should within emacs, i.e. move to
;; the beginning or end of the line, respectively.
(define-key global-map [home] 'move-beginning-of-line)
(define-key global-map [end] 'end-of-line)

;; Open the current file as root.
(defun current-as-root ()
  "Reopen current file as root."
  (interactive)
  (set-visited-file-name (concat "/sudo::" (buffer-file-name)))
  (setq buffer-read-only nil))

;; Have a key for loading the init file quickly.
(defun open-init-el ()
  "Open init.el quickly for editing."
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c i") 'open-init-el)

;; Bind M-o to what normally requires C-x o.
(global-set-key (kbd "M-o") 'other-window)

;; Have better buffer names for equally named files.
(require 'uniquify)

;; Bind fill-paragraph to C-c M-q
(global-set-key (kbd "C-c M-q") 'fill-paragraph)


;;;-----------------------------------------------------------------------------
;;; Emacs automagically managed settings. Clean up once in a while.
;;;-----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode window-numbering which-key use-package undo-tree smex scala-mode sbt-mode restclient projectile nyan-mode mustache-mode monokai-theme magit lsp-ui lsp-metals ido-vertical-mode ido-completing-read+ hl-todo goto-last-change git-gutter flycheck-joker flx-ido expand-region exec-path-from-shell dracula-theme company-lsp clj-refactor auto-indent-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
