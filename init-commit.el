;;;; A small init for writing commit messages fast. Maybe replace this later
;;;; with emacs in server mode. Or maybe remove it altogether if the magit
;;;; package suits my needs.
;;;;
;;;; Start this with `emacs -q -l ~/.emacs.d/init-commit.el`.

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(set-fill-column 70)
(menu-bar-mode -1)
