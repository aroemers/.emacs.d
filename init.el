;;;-----------------------------------------------------------------------------
;;; Convenient package handling in emacs
;;;-----------------------------------------------------------------------------

(require 'package)

;; Add automatically parsed versiontracking repositories.
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;; Make sure a package is installed
(defun package-require (package)
  "Install a package unless it is already installed
   or a feature with the same name is already active.

   Usage: (package-require 'package)"
  ;; Try to activate the package with at least version 0.
  (package-activate package '(0))
  ;; Try to just require the package. Maybe the user has it in his local config.
  (condition-case nil
      (require package)
    ;; If we cannot require it, it does not exist, yet. So install it.
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
(package-require 'monokai-theme)
(load-theme 'monokai t)

;; Set the color of some other parts of Emacs.
(set-face-background 'cursor "chartreuse1")

;; Disable the menubar, toolbar, scrollbars and set a decent size for the window
;; when using the window system.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when window-system
  ;; (set-frame-size (selected-frame) 179 53)
  (toggle-frame-maximized)
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 130))
  (setq-default line-spacing 3))


;;;-----------------------------------------------------------------------------
;;; Clojure related
;;;-----------------------------------------------------------------------------

;; Require the Clojure and Cider package.
(package-require 'clojure-mode)
(package-require 'cider)

;; Make sure paredit is active when clojure mode is active.
(package-require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Add clj-refactor
(package-require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas-minor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")))

;; Add better indentation for some symbols.
(define-clojure-indent
  (try-let 1)
  (with-resource 'defun)
  (fact 1)
  (facts 1)
  (guard-let 1)
  (cond-let 1)
  (asserts 1)
  (if-let* 1)
  (when-let* 1)
  (forcat 1)
  (go-loop 1)
  (dosync 0)
  (match-form 3)
  (routing 1)
  (rmap 1)
  (defstate 1)
  (fnp 1))

;; Bind fill-paragraph to C-c M-q
(global-set-key (kbd "C-c M-q") 'fill-paragraph)

;; Also enable Clojure mode for a .boot file.
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

;; Add joker linter
(package-require 'flycheck-joker)


;;;-----------------------------------------------------------------------------
;;; Emacs Lisp mode
;;;-----------------------------------------------------------------------------

;; Have Paredit enabled while in emacs-lisp mode.
(package-require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Have documentation in emacs-lisp.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


;;;-----------------------------------------------------------------------------
;;; Company mode
;;;-----------------------------------------------------------------------------

(package-require 'company)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

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

;; Disabled for now, as working on others' files may yield too big diffs.
;; (defun untabify-maybe ()
;;   (when (not indent-tabs-mode)
;;     (untabify (point-min) (point-max))))

;; (add-hook 'before-save-hook 'untabify-maybe)


;;;----------------------------------------------------------------------------
;;; Ido Mode related
;;;----------------------------------------------------------------------------

;; Enable ido mode. Remember that C-j uses the current selection in the ido
;; minibuffer, which comes in handy when selecting a dired directory (M-x dired).
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Have a better ido-everywhere.
(package-require 'ido-completing-read+)
(ido-ubiquitous-mode t)

;; Align the options vertically, and make up-down for options, and left-right
;; for history.
(package-require 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; Have a better fuzzy matching, and highlight the matching characters.
(package-require 'flx-ido)
(flx-ido-mode 1)

;; Have ido like completions for M-x (execute-extended-command).
;; It also gives a M-X shortcut to only show the commands from the
;; currend major mode.
(package-require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;;-----------------------------------------------------------------------------
;;; Term related settings.
;;;-----------------------------------------------------------------------------

;; Clear ansi-term buffer with C-c C-b (Clear Buffer),
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-c C-b")
              (lambda ()
                (interactive)
                (term-reset-terminal)))))

;; Have utf-8 encoding in terminals.
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)


;;;-----------------------------------------------------------------------------
;;; JavaScript mode
;;;-----------------------------------------------------------------------------

(package-require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))


;;;-----------------------------------------------------------------------------
;;; CSS/SCSS mode
;;;-----------------------------------------------------------------------------

(setq css-indent-offset 2)


;;;-----------------------------------------------------------------------------
;;; Other niceties
;;;-----------------------------------------------------------------------------

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
(global-set-key (kbd "M-a") 'mc/mark-all-like-this-dwim)

;; Go straight to the *scratch* buffer, i.e. skip the help message. And set a
;; nice welcoming message.
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking, Arnout!\n\n")

;; Show column number next to linenumber in the status bar.
(column-number-mode t)

;; Wrap words instread of breaking them.
(toggle-word-wrap t)

;; Show matching paren.
(show-paren-mode t)

;; Go to the last change, using M-l.
(package-require 'goto-last-change)
(global-set-key (kbd "M-l") 'goto-last-change)

;; Save backups and autosaves in the system's temporary directory.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Switch back to previous buffer, with C-c b.
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) nil)))
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; Don't require two spaces between sentences when moving with M-e and M-a.
(setq sentence-end-double-space nil)

;; Add projectile, e.g. for quickly opening files and searching in files.
(package-require 'projectile)
(projectile-global-mode t)
(setq shell-file-name "/bin/sh") ; in order to make rgrep work

;; Automatic indention on RET and yanks, and only one space on forward-kills.
(package-require 'auto-indent-mode)
(auto-indent-global-mode t)
(add-to-list 'auto-indent-disabled-modes-list 'cider-mode)
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
(global-set-key (kbd "C-x g") 'magit-status)

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
  "Reopen current file as root"
  (interactive)
  (set-visited-file-name (concat "/sudo::" (buffer-file-name)))
  (setq buffer-read-only nil))

;; Have a key for loading the init file quickly.
(global-set-key (kbd "C-c i")
                (lambda () (interactive) (find-file-other-window "~/.emacs.d/init.el")))

;; Have too long lines highlighted.
(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Have window numbers for faster switching.
(package-require 'window-numbering)
(window-numbering-mode t)

;; Bind M-o to what C-x o is bound to.
(global-set-key (kbd "M-o") (key-binding (kbd "C-x o")))

;; Have better buffer names for equally named files.
(require 'uniquify)

;; Add a markdown mode
(package-require 'markdown-mode)

;; Add nyan cat.
(package-require 'nyan-mode)
(nyan-mode 1)

;; Bind C-c c to compile.
(global-set-key (kbd "C-c C-c") 'recompile)

;; Bind C-c s to to helm git grep
(global-set-key (kbd "C-c s") 'helm-grep-do-git-grep)

;; Install yaml-mode
(package-require 'yaml-mode)

;; Install avy navigation
(package-require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

;; Display bound keys
(package-require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; Enable flycheck for all global modes
(global-flycheck-mode t)
(setq flycheck-global-modes t)

;; Set exec-path from shell
(package-require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "JAVA_HOME"))


;;;-----------------------------------------------------------------------------
;;; Emacs automagically managed settings. Clean up once in a while.
;;;-----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background t)
 '(avy-keys (quote (97 114 115 116 100 104 110 101 105)))
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))")
 '(cljr-favor-prefix-notation nil)
 '(package-selected-packages
   (quote
    (puppet-mode request tide rainbow-mode dockerfile-mode moody mustache-mode flycheck-joker yaml-mode window-numbering which-key undo-tree smex projectile ox-ioslide nyan-mode monokai-theme markdown-mode magit ido-vertical-mode ido-ubiquitous groovy-mode goto-last-change git-timemachine git-gutter flx-ido expand-region erc-hl-nicks company clojure-cheatsheet clj-refactor avy auto-indent-mode adoc-mode)))
 '(projectile-use-git-grep t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
