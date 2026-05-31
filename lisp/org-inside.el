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
;; hidden text on demand is also provided and added to the
;; context-dependent ctrl-c ctrl-c hook; see
;; `org-inside-toggle-hidden'.
;;
;; This mode is intended to be used with
;; e.g. `org-hide-emphasis-markers', and/or `org-highlight-links'
;; (with `bracket') to make editing the ends of links and emphasized
;; text more precise.

;;; Code:
(require 'org)
(require 'org-element)
(require 'face-remap)
(require 'cus-start) ; ensure 'cursor-type has its 'custom-type set
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

All appearance keys are optional, and can be freely combined.  If
`org-inside-appearance' is nil, no appearance changes will be applied
when point is inside hidden markers, but the command
`org-inside-toggle-hidden' can still be used to unhide hidden markers
for the entity you are inside."
  :group 'org-appearance
  :type `(choice
          (const :value nil :tag "No appearance changes: unhide on command only")
          (plist
           :tag "Specific appearance options"
	   :options
	   ((:cursor ,(get 'cursor-type 'custom-type)
		     :tag "Cursor Type")
	    (:face (choice (face :tag "Face Name")
			   (plist :tag "Attribute List"))
		   :tag "Text Face")
	    (:unhide boolean :tag "Unhide hidden markers"))))
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (when (featurep 'org-inside) (org-inside--reset-all))))

(defsubst org-inside--elem-at-point ()
  "Return the relevant org-entity with possible hidden contents at point."
  (when-let* ((ctx (org-element-context)))
    (org-element-lineage ctx '( link bold code italic verbatim
                                underline strike-through) t)))

(defun org-inside--overlay-modification (ov after-p &rest _r)
  "Detect modifications of the entity covered by `org-inside' overlay OV.
AFTER-P is t if called after the modification occurs.  If the org entity
covered by OV no longer exists, we set the appearance for outside."
  (when after-p
    (save-excursion
      (goto-char (overlay-start ov))
      (unless (org-inside--elem-at-point)
        (org-inside--set-appearance (selected-window))))))

(defun org-inside--overlay (win face unhide)
  "Return an appropriately styled overlay for window WIN.
FACE and UNHIDE are the text face and invisibility status; see
`org-inside-appearance'."
  (let ((ov (window-parameter win 'org-inside-overlay)))
    (unless (and ov (overlayp ov) (buffer-live-p (overlay-buffer ov)))
      (setq ov (make-overlay 1 1 (window-buffer win) t))
      (overlay-put ov 'window win)
      (overlay-put ov 'cursor-sensor-functions '(org-inside--sensor))
      (overlay-put ov 'modification-hooks '(org-inside--overlay-modification))

      ;; For auto-unhiding, we set the invisible property to something
      ;; guaranteed not to be on the `buffer-invisibility-spec'.
      (when unhide (overlay-put ov 'invisible 'org-inside--not-hidden))
      (when face (overlay-put ov 'face face))
      (set-window-parameter win 'org-inside-overlay ov))
    ov))

(defun org-inside--set-appearance (win &optional beg end)
  "Set appearance and hide state for hidden-marker text.
The text is from BEG to END in the window WIN's buffer.  If BEG or END
are nil, point is considered to be outside the text and the prior
appearance is restored.

Note that if the `cursor-type' is configured to change inside (see
`org-inside-appearance') but the `window-cursor-type' is currently
nil (i.e. the cursor is hidden), the cursor is left hidden, and the
window parameter `pending-cursor-type' is set instead.  Other tools can
consult this window parameter to restore the cursor type."
  (cl-destructuring-bind ( &key cursor face unhide) org-inside-appearance
    (let* ((inside-p (and beg end))
           (ov (org-inside--overlay win face unhide))
           (showing-p (overlay-get ov 'invisible))) ; non-nil = unhidden!
      ;; We move the overlay when returning to the run-loop to avoid
      ;; the cursor-sensor race for point adjustment, since our
      ;; overlay unhides text which point adjustment can skip.  As
      ;; well, since both text and overlay implement
      ;; cursor-sensor-functions, this avoids a similar race as to
      ;; which applies.
      (run-at-time 0 nil (lambda (buf)
                           (with-current-buffer buf
                             (if inside-p (move-overlay ov beg end)
                               (delete-overlay ov))))
                   (current-buffer))
      ;; more natural movement moving out when hidden text is visible
      (unless (or (not showing-p) inside-p)
        (setq disable-point-adjustment t))
      ;; User may have toggled hiding; re-set
      (when (not inside-p)
        (overlay-put ov 'invisible (and unhide 'org-inside--not-hidden)))
      (when cursor
        (let ((cursor (if inside-p
                          cursor
                        (or (window-parameter win 'org-inside-old-cursor) t)))
              (win-cursor-type (window-cursor-type win)))
          (if (eq win-cursor-type nil)
	      ;; Do not override a hidden (nil) cursor; set it pending instead
              (set-window-parameter win 'pending-cursor-type cursor)
            (unless (eq cursor win-cursor-type) ; guard against double entry
              (when inside-p            ; save the outside cursor type
	        (set-window-parameter win 'org-inside-old-cursor
                                      win-cursor-type))
              (set-window-cursor-type win cursor))))))))

(defun org-inside--sensor (win _pos type)
  "Handle cursor appearance and unhiding inside hidden text wrapped entities.
To be set via the `cursor-sensor-functions' property on hidden-marker
text, as well as the overlay returned by `org-inside--overlay' .  WIN
POS, and TYPE are the window, former position, and cursor movement
type."
  (cond
   ((eq type 'entered)         ; called from the in-text cursor-sensor
    (when-let* ((elem (org-inside--elem-at-point))
                (beg (org-element-begin elem))
                (end (- (org-element-end elem) (org-element-post-blank elem))))
      (unless (and (eq (org-element-type elem) 'link)
                   (not org-link-descriptive))
        (org-inside--set-appearance win beg end))))
   ((eq type 'left)          ; called from the overlay's cursor-sensor
    (org-inside--set-appearance win))))

(defsubst org-inside--restore-cursor (win)
  "Restore old cursor in WIN (if any).
If the current window cursor type is nil (i.e. the cursor is hidden), no
change is made."
  (when-let* ((old-type (or (window-parameter win 'org-inside-old-cursor)
                            (window-parameter win 'pending-cursor-type)))
              (type (window-cursor-type win)))
    (set-window-cursor-type win old-type)
    (set-window-parameter win 'org-inside-old-cursor nil)))

(defsubst org-inside--clear-overlay (win)
  "Clear the `org-inside' overlay from window WIN."
  (when-let* ((ov (window-parameter win 'org-inside-overlay))
              (_ (overlayp ov)))
    (delete-overlay ov)
    (set-window-parameter win 'org-inside-overlay nil)))

(defsubst org-inside--in-hidden-marker-text (&optional pos)
  "Return non-nil if inside hidden marker text.
If POS is nil, use point."
  (and-let* ((csf (get-text-property (or pos (point))
                                     'cursor-sensor-functions))
             (_ (memq 'org-inside--sensor csf)))))

(defvar org-inside-mode)
(defun org-inside--buffer-changed (win)
  "Handle `org-inside' buffers appearing in window WIN."
  (with-current-buffer (window-buffer win)
    (when org-inside-mode
      (if (org-inside--in-hidden-marker-text)
          (org-inside--sensor win nil 'entered)
        (org-inside--sensor win nil 'left)))))

(defun org-inside--frame-changed (frame)
  "Handle window buffer change for all windows on FRAME."
  (walk-windows
   (lambda (win)
     (unless (buffer-local-value 'org-inside-mode (window-buffer win))
       (org-inside--clear-overlay win)
       (org-inside--restore-cursor win)))
   nil frame))

(defun org-inside--add-properties (type _beg _end visible-beg visible-end)
  "Add text properties to invisible text for org-inside functionality.
TYPE is the type of text being hidden.  BEG, END, VISIBLE-BEG,
VISIBLE-END are the buffer positions of the affected text and its
visible portion."
  ;; Emacs 31+ fires cursor-sensor at positions where an inserted
  ;; character would inherit the `cursor-sensor-function' property
  ;; (including rear stickiness).  Prior versions do not respect
  ;; stickiness.  To get the same functionality, we include the next
  ;; trailing (marker) char in the "inside" region in earlier
  ;; versions, but do not inherit the sensor property for characters
  ;; inserted after it.
  (when (< emacs-major-version 31)      ;; TODO: use static-if when available
    (setq visible-end (min (1+ visible-end) (point-max)))
    (add-text-properties (1- visible-end) visible-end
                         '(rear-nonsticky (cursor-sensor-functions))))
  ;; for proper point adjustment
  (when (eq type 'emphasis)
    (org-rear-nonsticky-at visible-beg)
    (when (< emacs-major-version 31)
      (org-rear-nonsticky-at visible-end)))
  (put-text-property visible-beg visible-end
		     'cursor-sensor-functions '(org-inside--sensor)))

(defun org-inside--setup ()
  "Setup buffer for `org-inside'."
  (cursor-sensor-mode 1)
  (cl-pushnew 'cursor-sensor-functions
              (buffer-local-value 'org-extra-unfontify-properties
                                  (current-buffer)))
  (add-hook 'window-buffer-change-functions #'org-inside--buffer-changed nil t)
  (add-hook 'org-hidden-text-functions #'org-inside--add-properties nil t)
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-inside-toggle-hidden nil t)
  (font-lock-flush) ;; does not call sensor functions
  (dolist (w (get-buffer-window-list nil nil t))
    (org-inside--buffer-changed w)))

(defun org-inside--teardown ()
  "Tear down `org-inside-mode' in buffer."
  (dolist (w (get-buffer-window-list nil nil t))
    (org-inside--restore-cursor w)
    (org-inside--clear-overlay w))
  (cursor-sensor-mode -1)
  (setq-local org-extra-unfontify-properties
              (delq 'cursor-sensor-functions org-extra-unfontify-properties))
  (remove-hook 'org-hidden-text-functions #'org-inside--add-properties t)
  (remove-hook 'window-buffer-change-functions #'org-inside--buffer-changed t)
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-inside-toggle-hidden t))

(defun org-inside--reset-all ()
  "Reset org-inside in all `org-inside' buffers."
  (walk-windows
   (lambda (win)
     (when (window-parameter win 'org-inside-overlay)
       (with-selected-window win
         (org-inside--clear-overlay win)
         (org-inside--restore-cursor win)
         (org-inside--buffer-changed win))))
   nil t))

(defun org-inside-toggle-hidden ()
  "Toggle visibility of hidden text for entity at point.
Operates only when inside an entity wrapped by hidden text and
`org-inside-mode' is enabled.  Text will be re-hidden when point leaves
the entity.  See `org-inside-appearance' to enable automatic unhiding or
configure other appearance settings.  Returns non-nil if the visibility
was toggled, making it suitable for inclusion on
`org-ctrl-c-ctrl-c-hook'."
  (interactive)
  (and-let* ((ov (window-parameter nil 'org-inside-overlay))
             (_ (overlay-buffer ov)))
    (let ((inv (overlay-get ov 'invisible)))
      (overlay-put ov 'invisible
                   (if inv nil 'org-inside--not-hidden)))
    t))

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

Hidden text can be unhidden, either automatically, or by using the
command `org-inside-toggle-hidden'.  See `org-inside-appearance' to
configure what appearance changes occur."
  :global nil
  (cond
   ((and org-inside-mode
         (not (cl-loop for key in org-inside-appearance by #'cddr
                       always (memq key '(:cursor :face :unhide)))))
    (setq org-inside-mode nil)
    (user-error "`org-inside-appearance' malformed"))
   (org-inside-mode (org-inside--setup))
   (t (org-inside--teardown))))

(add-hook 'window-buffer-change-functions #'org-inside--frame-changed)

(provide 'org-inside)
;;; org-inside.el ends here
