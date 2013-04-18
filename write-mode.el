;;;; Write mode, by Arnout Roemers.
;;;;
;;;; This mode sets horizontal margins useful when writing, where less wide lines
;;;; improve the readability.
;;;;
;;;; fixme: When resizing window, the narrow-margins function does not seem to
;;;;        be called.
;;;; fixme: A new window (e.g. after C-x 3) has funny margins, only to be fixed
;;;;        after a window size change.

(defun narrow-margins (frame)
  (set-window-margins nil 
		      (/ (- (window-total-width) 120) 2) 
		      (/ (- (window-total-width) 120) 2)))

(defun normal-margins ()
  (set-window-margins nil nil nil))

(define-minor-mode write-mode
  "Toggle the write mode.
This mode sets horizontal margins useful when writing, where less wide lines
improve the readability."
  :lighter " Write"
  :global t
  (if write-mode
      (progn
	(add-hook 'window-size-change-functions 'narrow-margins)
	(narrow-margins nil))
    (progn
      (normal-margins)
      (remove-hook 'window-size-change-functions 'narrow-margins))))
