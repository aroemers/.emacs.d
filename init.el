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
(package-require 'molokai-theme)
(load-theme 'molokai t)

;; Set the color of some other parts of Emacs.
(set-face-background 'mode-line "#ddddee") ; the text color.
(set-face-foreground 'mode-line "#337744") ; the bar color.
(set-face-background 'cursor "chartreuse1")

;; Disable the menubar, toolbar, scrollbars and set a decent size for the window
;; when using the window system.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when window-system
  (set-frame-size (selected-frame) 200 56)
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 130)))


;;;-----------------------------------------------------------------------------
;;; Highlight top-level comment blocks in lisps
;;;-----------------------------------------------------------------------------

(require 'cl)

(defun overlays-with-property-in (begin end property &optional value)
  "Return the overlays that overlap with the region begin to end, having a the
  specified property. A fourth, optional, argument is the expected value of that
  property. Note that an overlay from 1 to 3 is only found when the range
  begin-end covers 2 (the behaviour of the standard overlays-in function)."
  (let ((overlays (overlays-in begin end)))
    (cl-remove-if-not (lambda (overlay)
                        (let ((propvalue (overlay-get overlay property)))
                          (and propvalue
                               (or (not value) (equal value propvalue)))))
                      overlays)))

(defface hl-comment-block-face
  '((((background light)) (:background "burlywood2" :foreground "burlywood4"))
    (((background dark)) (:background "#222628" :foreground "#cccc77")))
  "Face for comment overlay blocks.")

(defun hl-comment-block (end)
  "Searches for the first occurrence of a toplevel ;;; comment, starting from
  point. If no occurrence is found, nil is returned. Otherwise, a highlighting
  overlay is added to the comment line if it does not have one already. A non-nil
  value is returned in this case."
  (when (re-search-forward "^;;;" end t)
    (if (not (overlays-with-property-in (point) (point) 'for-comments))
        (let ((start (- (point) 3)))
          (end-of-line)
          (let* ((end (+ 1 (point)))
                 (overlay (make-overlay start end)))
            (overlay-put overlay 'face 'hl-comment-block-face)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'priority 999)
            (overlay-put overlay 'for-comments t))
          (goto-char (+ start 3)))
      t)))

(defun hl-comment-block-before-change (begin end)
  "Removes comment highlighting overlays in the region that is about to change."
  (mapc 'delete-overlay (overlays-with-property-in begin end 'for-comments)))

(defun hl-comment-block-after-change (begin end length)
  "Executes hl-comment-block, starting from the beginning of the line of
  the beginning of the changed region."
  (save-excursion
    (goto-char begin)
    (beginning-of-line)
    (while (hl-comment-block (+ end 3)))))

(defun hl-comment-block-enable ()
  "Enable highlighting top-level comment blocks."
  (add-hook 'before-change-functions 'hl-comment-block-before-change nil t)
  (add-hook 'after-change-functions 'hl-comment-block-after-change nil t)
  (save-excursion
    (goto-char (point-min))
    (while (hl-comment-block (point-max)))))


;;;-----------------------------------------------------------------------------
;;; Clojure and nrepl modes
;;;-----------------------------------------------------------------------------

;; Require the Clojure packages.
(package-require 'clojure-mode)
(package-require 'nrepl)
(package-require 'nrepl-ritz)

;; Make sure paredit is active when clojure mode is active.
(package-require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

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
  (forcat 1))

;; Have highlighted comment blocks.
(add-hook 'clojure-mode-hook 'hl-comment-block-enable)

;; Hack for displaying ansi colors correctly for all response handlers in the
;; nrepl buffer. Remove when fixed in later version of nrepl.el.
;; From https://github.com/kingtim/nrepl.el/pull/275.
(defun nrepl-emit-output (buffer string &optional bol)
  "Using BUFFER, emit STRING.
  If BOL is non-nil, emit at the beginning of the line."
  (with-current-buffer buffer
    (nrepl-emit-output-at-pos buffer string nrepl-input-start-mark bol)
    (ansi-color-apply-on-region (marker-position nrepl-output-start) (point-max))))


;;;-----------------------------------------------------------------------------
;;; Scala mode
;;;-----------------------------------------------------------------------------

;; Require the needed packages and make sure it is active when a Scala file is
;; loaded. Note that there is also a Scala-mode2, which I must check out some day.
(package-require 'scala-mode)
(require 'scala-mode-auto)


;;;-----------------------------------------------------------------------------
;;; Emacs Lisp mode
;;;-----------------------------------------------------------------------------

;; Have Paredit enabled while in emacs-lisp mode.
(package-require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Have highlighted comment blocks in emacs-lisp.
(add-hook 'emacs-lisp-mode-hook 'hl-comment-block-enable)

;; Have documentation in emacs-lisp.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


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

;; Set some options for ERC.
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setq erc-server-reconnect-timeout 60)

;; Scroll down to the bottom automatically.
(add-hook 'erc-mode-hook 'erc-scrolltobottom-mode)

;; Auto-connect when a password is given.
(defun irc-maybe ()
  "Connect to IRC, when a password is given."
  (let ((p (read-passwd "Give password to connect to FreeBNC IRC: ")))
    (when (> (length p) 0)
      (erc-tls :server "ie.freebnc.net" :port 6697
               :nick "aroemers" :password (concat "aroemers:" p)))))

;; Ask to auto-connect on startup.
(add-hook 'emacs-startup-hook 'irc-maybe)

;; Have colorised nick-names.
(package-require 'erc-hl-nicks)


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

;; Have ido like completions for M-x (execute-extended-command).
;; It also gives a M-X shortcut to only show the commands from the
;; currend major mode.
(package-require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

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
(toggle-word-wrap t)

;; Show matching paren.
(show-paren-mode t)

;; Go to the last change, using M-l.
(package-require 'goto-chg)
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

;; Integrate with the system's clipboard when in a terminal. Does not seem to
;; work in the terminal on OS X Mountain Lion, while it does work in a terminal
;; on OS X Lion.
(when (not window-system)
  (package-require 'simpleclip)
  (simpleclip-mode t))

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

;; Change magit diff colors.
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-background 'magit-item-highlight "#131315")))



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
(package-require 'window-number)
(autoload 'window-number-mode "window-number" t)
(autoload 'window-number-meta-mode "window-number" t)
(window-number-mode t)
(window-number-meta-mode t)

;; Bind M-o to what C-x o is bound to.
(global-set-key (kbd "M-o") (key-binding (kbd "C-x o")))


;;;-----------------------------------------------------------------------------
;;; Emacs automagically managed settings. Don't touch :)
;;;-----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-git-editor "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
 '(org-capture-templates (quote (("o" "Add item in Online Touch inbox." item (file+headline "~/onlinetouch/ottododone.org" "Inbox") "")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
