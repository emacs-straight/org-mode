;;; org-inside.el --- Change appearance inside hidden markers  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Free Software Foundation, Inc.
;;
;; Author:  J.D. Smith <jdtsmith at gmail dot com>
;; Maintainer: J.D. Smith <jdtsmith at gmail dot com>
;; Keywords: folding, invisible text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; In org, text can be surrounded by hidden markers.  When "inside"
;; such text, `org-inside' can change the cursor type, text face,
;; and/or optionally (temporarily) unhide the hidden markers.  This is
;; especially valuable when adjacent to such text, because point can
;; be ambiguous in these positions: at the same apparent cursor
;; position, point can be either inside or outside the hidden markers.
;;
;; This mode can be used with e.g. `org-hide-emphasis-markers', or
;; `org-highlight-links' (with `bracket') to make editing the ends of
;; links and emphasized text more precise.

;;; Code:
(require 'org)
(require 'face-remap)
(eval-when-compile (require 'cl-lib))

(defcustom org-inside-appearance '(:cursor bar)
  "Special appearance when point is inside hidden markers.
Appearance changes can include cursor type, wrapped text face, and
marker unhiding.  The value is a plist, with possible keys and values:

 `:cursor': one of the possible `cursor-type' types to change to

 `:face': an optional face (or anonymous list of face attributes) to
     apply to the text

 `:unhide': a boolean indicating whether to temporarily un-hide the
     hidden markers

All keys are optional, and can be freely combined."
  :group 'org-appearance
  :type `(plist :options ((:cursor ,(get 'cursor-type 'custom-type)
                                   :tag "Cursor Type")
                          (:face (choice (face :tag "Face Name")
                                         (plist :tag "Attribute List"))
                                 :tag "Cursor Face")
                          (:unhide boolean :tag "Unhide hidden markers"))))

(defun org-inside--overlay (win face unhide)
  "Return an appropriate overlay for window WIN.
FACE and UNHIDE are the text face and invisibility status; see
`org-inside-appearance'."
  (let ((ov (window-parameter win 'org-inside-overlay)))
    (unless (and ov (overlayp ov) (buffer-live-p (overlay-buffer ov)))
      (setq ov (make-overlay 0 0 (window-buffer win) nil t))
      (overlay-put ov 'window win)
      ;; For unhiding, we set the invisible property to something
      ;; guaranteed not to be on the `buffer-invisibility-spec'.
      (when unhide (overlay-put ov 'invisible 'org-inside--not-hidden))
      (when face (overlay-put ov 'face face))
      (set-window-parameter win 'org-inside-overlay ov))
    ov))

(defun org-inside--set-appearance (win beg end)
  "Set appearance and hide state for hidden-marker text.
The text is from BEG to END in the window WIN's buffer.  If both BEG and
END are equal to 0, point is considered to be outside the text and the
prior appearance is restored.

Note that if the `cursor-type' is configured to change inside (see
`org-inside-appearance') but the `window-cursor-type' is currently
nil (i.e. the cursor is hidden), the cursor is left hidden, and the
window parameter `pending-cursor-type' is set instead.  Other tools can
consult this window parameter to restore the cursor type."
  (cl-destructuring-bind ( &key cursor face unhide
                           &aux (moved-inside-p (or (> beg 0) (> end 0))))
      org-inside-appearance
    (when (or unhide face)
      (move-overlay (org-inside--overlay win face unhide) beg end)
      ;; more natural movement when markers are visible
      (when unhide
        (unless moved-inside-p (setq disable-point-adjustment t))))
    (when cursor
      (let ((cursor (if moved-inside-p
                        cursor
                      (or (window-parameter win 'org-inside-old-cursor) t)))
            (win-type (window-cursor-type win)))
        (if (eq win-type nil)
	    ;; Do not override a hidden cursor; just set pending instead
            (set-window-parameter win 'pending-cursor-type cursor)
          (when moved-inside-p                  ; save the outside cursor type
	    (set-window-parameter win 'org-inside-old-cursor win-type))
          (set-window-cursor-type win cursor))))))

(defun org-inside--sensor (win _pos type)
  "Handle cursor appearance and unhiding inside hidden-marker entities.
To be set via the `cursor-sensor-functions' property on hidden-marker
text.  WIN and TYPE are the window and cursor movement type."
  (cond
   ((eq type 'entered)
    (let ((beg (1- (previous-single-property-change
                    (1+ (point)) 'cursor-sensor-functions)))
          (end (next-single-property-change
                (point) 'cursor-sensor-functions)))
      (org-inside--set-appearance win beg end)))
   ((eq type 'left)
    (org-inside--set-appearance win 0 0))))

(defsubst org-inside--restore-cursor (&optional win)
  "Restore old cursor in WIN (if any).
If the current window cursor type is nil (i.e. the cursor is hidden), no
change is made."
  (when-let* ((old-type (window-parameter win 'org-inside-old-cursor))
              (type (window-cursor-type win)))
    (set-window-cursor-type win old-type)
    (set-window-parameter win 'org-inside-old-cursor nil)))

(defsubst org-inside--clear-overlay (&optional win)
  "Clear the `org-inside' overlay from window WIN."
  (when-let* ((ov (window-parameter win 'org-inside-overlay))
              (_ (overlayp ov)))
    (delete-overlay ov)
    (set-window-parameter win 'org-inside-overlay nil)))

(defvar 'org-inside-mode)

;; Not needed for v31+
(defun org-inside--frame-changed (frame)
  "Handle window buffer change for windows on FRAME."
  (dolist (win (window-list frame))
    (when-let* ((buf (window-buffer win))
                (_ (not (and buf (buffer-local-value 'org-inside-mode buf)))))
      (org-inside--clear-overlay win)
      (org-inside--restore-cursor win))))

(defun org-inside--maybe-sense (&optional win)
  "Handle `org-inside' buffers appearing in window WIN."
  (with-current-buffer (window-buffer win)
    (when-let* ((csf (get-text-property (point) 'cursor-sensor-functions))
                (_ (memq 'org-inside--sensor csf)))
      (org-inside--sensor win (point) 'entered))))

(defun org-inside--add-emphasis-props ()
  "Add text properties to emphasized text for org-inside functionality."
  (put-text-property (match-beginning 4) (match-end 2)
		     'cursor-sensor-functions '(org-inside--sensor))
  (org-rear-nonsticky-at (match-end 3)))

(defun org-inside--setup ()
  "Setup buffer for `org-inside'."
  (cursor-sensor-mode 1)
  (cl-pushnew 'cursor-sensor-functions
              (buffer-local-value 'org-extra-unfontify-properties
                                  (current-buffer)))
  (add-hook 'window-buffer-change-functions #'org-inside--maybe-sense nil t)
  (add-hook 'org-do-emphasis-hook #'org-inside--add-emphasis-props nil t)
  (font-lock-flush) ;; does not call sensor functions
  (org-inside--maybe-sense))

(defun org-inside--teardown ()
  "Tear down `org-inside-mode' in buffer."
  (org-inside--restore-cursor)
  (org-inside--clear-overlay)
  (cursor-sensor-mode -1)
  (setq-local org-extra-unfontify-properties
              (delq 'cursor-sensor-functions org-extra-unfontify-properties))
  (remove-hook 'window-buffer-change-functions #'org-inside--maybe-sense t)
  (remove-hook 'org-do-emphasis-hook #'org-inside--add-emphasis-props t))

;;;###autoload
(define-minor-mode org-inside-mode
  "Change appearance when point is inside hidden markers.
The cursor type and/or text face can be altered when point is inside the
markers.  Additionally, the markers can be temporarily unhidden.  See
`org-inside-appearance' to configure what appearance changes occur."
  :global nil
  (cond
   ((and org-inside-mode
         (not (and org-inside-appearance
                   (cl-loop for key in org-inside-appearance by #'cddr
                            always (memq key '(:cursor :face :unhide))))))
    (setq org-inside-mode nil)
    (user-error "`org-inside-appearance' malformed"))
   (org-inside-mode (org-inside--setup))
   (t (org-inside--teardown))))

;; N.B. this will not be needed in v31+, as buffer-local
;; window-buffer-change-functions for a buffer appearing and
;; disappearing:
(add-hook 'window-buffer-change-functions #'org-inside--frame-changed)

(provide 'org-inside)
;;; org-inside.el ends here
