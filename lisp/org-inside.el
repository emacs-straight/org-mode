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

;; In org, entities including emphasized text and links can have
;; hidden flanking components.  When the cursor is adjacent to such
;; entities, point can be ambiguous: at the same apparent cursor
;; position, point can be either inside or outside the hidden regions.
;; This can make it hard to edit precisely.
;;
;; To help with this problem, `org-inside' changes the appearance
;; "inside" such an entity, to make it clear where you are.
;; 
;; Appearance changes are highly configurable, and can include
;; changing cursor type, text face (e.g. adding a colorful underline),
;; and/or unhiding the hidden text.  A command to hide/unhide the
;; hidden text on demand is also provided; see
;; `org-inside-toggle-hidden'.
;; 
;; This mode is intended to be used with
;; e.g. `org-hide-emphasis-markers', and/or `org-highlight-links'
;; (with `bracket') to make editing the ends of links and emphasized
;; text more precise.

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

 `:unhide': a boolean indicating whether to automatically un-hide the
     hidden markers (unhiding can also be toggled by command; see
     `org-inside-toggle-hidden')

All appearance keys are optional, and can be freely combined."
  :group 'org-appearance
  :type `(plist
          :options
          ((:cursor ,(get 'cursor-type 'custom-type)
                    :tag "Cursor Type")
           (:face (choice (face :tag "Face Name")
                          (plist :tag "Attribute List"))
                  :tag "Cursor Face")
           (:unhide boolean :tag "Unhide hidden markers"))))

(defun org-inside--overlay (win face unhide)
  "Return an appropriately styled overlay for window WIN.
FACE and UNHIDE are the text face and invisibility status; see
`org-inside-appearance'."
  (let ((ov (window-parameter win 'org-inside-overlay)))
    (unless (and ov (overlayp ov) (buffer-live-p (overlay-buffer ov)))
      (setq ov (make-overlay 0 0 (window-buffer win) t))
      (overlay-put ov 'window win)
      (overlay-put ov 'cursor-sensor-functions '(org-inside--sensor))
      ;; For auto-unhiding, we set the invisible property to something
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
                           &aux (inside-p (or (> beg 0) (> end 0))))
      org-inside-appearance
    (let* ((ov (org-inside--overlay win face unhide))
           (showing-p (overlay-get ov 'invisible))) ; non-nil = unhidden!
      (message "Setting appearance with inside: %s showing: %s beg: %d end: %d"
               inside-p showing-p beg end)
      (run-at-time 0 nil (lambda () (move-overlay ov beg end)))
      ;; more natural movement moving out when hidden text is visible
      (unless (or (not showing-p) inside-p)
        (setq disable-point-adjustment t))
      ;; User may have toggled hiding; re-set
      (when (not inside-p)
        (overlay-put ov 'invisible (and unhide 'org-inside--not-hidden))))
    (when cursor
      (let ((cursor (if inside-p
                        cursor
                      (or (window-parameter win 'org-inside-old-cursor) t)))
            (win-cursor-type (window-cursor-type win)))
        (if (eq win-cursor-type nil)
	    ;; Do not override a hidden (nil) cursor; set it pending instead
            (set-window-parameter win 'pending-cursor-type cursor)
          (when inside-p          ; save the outside cursor type
	    (set-window-parameter win 'org-inside-old-cursor win-cursor-type))
          (set-window-cursor-type win cursor))))))

(defun org-inside--sensor (win _pos type)
  "Handle cursor appearance and unhiding inside hidden text wrapped entities.
To be set via the `cursor-sensor-functions' property on hidden-marker
text, as well as the overlay returned by `org-inside--overlay' .  WIN
and TYPE are the window and cursor movement type."
  (cond
   ((eq type 'entered) ; called from the in-text cursor-sensor
    (when-let*
        ((prop (cl-loop for prop in '(org-emphasis htmlize-link)
                        if (get-text-property (point) prop) return prop))
         (beg (previous-single-property-change (point) prop nil (point-min)))
         (end (next-single-property-change (point) prop nil (point-max))))
      (org-inside--set-appearance win beg end)))
   ((eq type 'left)  ; called from the overlay's cursor-sensor
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

(defun org-inside--add-link-props (_beg _end visible-beg visible-end)
  "Add text properties to bracket links for org-inside functionality.
BEG, END, VISIBLE-BEG, VISIBLE-END are the buffer positions of the link
text and its visible portion."
  (put-text-property visible-beg (1+ visible-end)
		     'cursor-sensor-functions '(org-inside--sensor)))

(defun org-inside--setup ()
  "Setup buffer for `org-inside'."
  (cursor-sensor-mode 1)
  (cl-pushnew 'cursor-sensor-functions
              (buffer-local-value 'org-extra-unfontify-properties
                                  (current-buffer)))
  (add-hook 'window-buffer-change-functions #'org-inside--maybe-sense nil t)
  (add-hook 'org-do-emphasis-hook #'org-inside--add-emphasis-props nil t)
  (when (memq 'bracket org-highlight-links)
    (add-hook 'org-activate-hidden-links-functions
              #'org-inside--add-link-props nil t))
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
  (remove-hook 'org-activate-hidden-links-functions
               #'org-inside--add-link-props t)
  (remove-hook 'org-do-emphasis-hook #'org-inside--add-emphasis-props t))

(defun org-inside-toggle-hidden ()
  "Toggle visibility of hidden text for entity at point.
Operates only when inside an entity wrapped by hidden text.  Text will
be re-hidden when point leaves the entity.  See `org-inside-appearance'
to enable automatic unhiding."
  (interactive)
  (when-let* ((ov (window-parameter nil 'org-inside-overlay))
              (_ (and (> (overlay-start ov) 0)
                      (> (overlay-end ov) 0))))
    (let ((inv (overlay-get ov 'invisible)))
      (overlay-put ov 'invisible
                   (if inv nil 'org-inside--not-hidden)))))

;;;###autoload
(define-minor-mode org-inside-mode
  "Change appearance when point is inside an entity wrapped by hidden text.
The cursor type and/or text face can be altered when point is inside the
hidden text.  \"Inside\" means characters entered at that point will
appear with the visible text.  For example, entering a character when
\"inside\" underlined text would make the new character underlined as
well:

  [x]_underline_   ; outside
  _[x]underline_   ; inside

Hidden text can be unhidden, either automatically, or by using
`org-inside-toggle-hidden'.  See `org-inside-appearance' to configure
what appearance changes occur."
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
