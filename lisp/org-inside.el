;;; org-inside.el --- Change appearance inside hidden contents entities  -*- lexical-binding: t; -*-
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

;; In org, entities including emphasized text and links can contain
;; hidden components (markers, brackets, link URL, etc.).  When the
;; cursor is adjacent to such hidden text, the point can be ambiguous:
;; at the same apparent cursor position, point can be either inside or
;; outside the hidden region.  This can make it hard to edit
;; precisely.
;;
;; To alleviate this problem, `org-inside' changes the appearance when
;; "inside" such an entity, to make it clear where you are.
;;
;; Appearance changes are highly configurable, and can include
;; changing the cursor type, text face (e.g. adding a colorful
;; underline), and/or automatically unhiding the hidden text.  A
;; command to hide/unhide the hidden text on demand is also provided
;; and added to the context-dependent ctrl-c ctrl-c hook; see
;; `org-inside-toggle-hidden'.
;;
;; This mode is intended to be used with e.g.
;; `org-hide-emphasis-markers', and/or `org-highlight-links' (with
;; `bracket' / `org-descriptive-links') to make editing links and
;; emphasized text easier.  If your version of Emacs supports it,
;; nested entities with hidden contents are supported, with face
;; changes applied to the innermost entity.

;;;; For Developers:
;; 
;; Note the shorthand substitution for window/overlay state structure
;; slots; see file-local-variables:
;;
;;    ois/  => org-inside-state-

;;; Code:
(require 'org)
(require 'org-element)
(require 'face-remap)
(require 'cus-start) ; ensure 'cursor-type has its 'custom-type set
(require 'cl-seq)

(defcustom org-inside-appearance '(:cursor bar)
  "Special appearance when point is inside text with hidden contents.
Appearance changes can include cursor type, text face, and unhiding.

The value is a plist, with possible keys and values:

 `:cursor': one of the possible `cursor-type' types to change to while
     inside.

 `:face': an optional face (or anonymous list of face attributes) to
     apply to the visible text of the innermost entity.

 `:unhide': a boolean indicating whether to automatically un-hide the
     hidden contents within the outermost entity at point.  Unhiding can
     also be toggled by command; see `org-inside-toggle-hidden'.

All appearance keys are optional, and can be freely combined.

If `org-inside-appearance' is nil, no appearance changes will be applied
when point is inside hidden contents entities.  The command
`org-inside-toggle-hidden' can still be used to unhide hidden text.

For nested entities with hidden contents, all appearance changes except
`:face' apply to the outermost entity.  `:face' is applied to the
innermost, when inside it (v31+ only)."
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

(defvar-local org-inside--hidden-contents-types nil
  "Entity types with hidden contents.")

(defun org-inside--setup-hidden-contents-types ()
  "Setup entity types with hidden contents."
  (let ((types '(bold code italic verbatim underline strike-through)))
    (when org-link-descriptive
      (setq types (cons 'link types)))
    (setq org-inside--hidden-contents-types types)))

(defun org-inside--elems-at-point ()
  "Return all org-entity with possible hidden contents at point.
The innermost elements are first on the list."
  (when-let* ((ctx (org-element-context)))
    (org-element-lineage-map ctx #'identity
      org-inside--hidden-contents-types t)))

(defun org-inside--overlay-modification (ov after-p &rest _r)
  "Detect modifications of the entity covered by `org-inside' overlay OV.
AFTER-P is t if called after the modification occurs.  If the org
entit(ies) covered by OV no longer exists, we set the appearance for
outside."
  (when after-p
    (save-excursion
      (goto-char (overlay-start ov))
      (unless (org-inside--elems-at-point)
        (org-inside--set-appearance (selected-window))))))

(defvar-local org-inside--states nil)
(cl-defstruct (org-inside-state
               (:copier nil)
               (:conc-name ois/)  ; Note: ois/ => org-inside-state-
               (:constructor nil)
               (:constructor ois/create))
  "State mapping from windows to `org-inside' overlays."
  ( window nil :type window
    :documentation "The window that overlays apply to.")
  ( ov nil :type overlay
    :documentation "PRIMARY overlay.")
  ( ov2 nil :type overlay
    :documentation "SECONDARY overlay (face only, if needed).")
  ( saved-cursor-type nil :type symbol
    :documentation "The saved cursor type in WIN."))

(defun org-inside--restore-cursor (win old-type)
  "Restore old cursor in WIN to OLD-TYPE (if any).
If OLD-TYPE is nil, use the `pending-cursor-type' parameter of WIN, if
any.  If the current window cursor type is nil (i.e. the cursor is
hidden), no change is made."
  (let* ((pending-type (window-parameter win 'pending-cursor-type))
         (old-type (or old-type pending-type)))
    (when (and old-type (window-cursor-type win))
      (set-window-cursor-type win old-type)
      (when pending-type
        (set-window-parameter win 'pending-cursor-type nil)))))

(defun org-inside--reset-state (state)
  "Delete overlays and restore cursor in the window indicated by STATE."
  (pcase-let (((cl-struct org-inside-state window ov ov2 saved-cursor-type)
               state))
    (when (window-live-p window)
      (org-inside--restore-cursor window saved-cursor-type))
    (when (overlayp ov) (delete-overlay ov))
    (when (overlayp ov2) (delete-overlay ov2))))

(defvar org-inside-mode)
(defun org-inside--trim-states (&optional all)
  "Remove and reset states that no long apply to the current buffer.
If ALL is non-nil, remove all states recorded for this buffer.
Otherwise, remove only stale states for non-live windows, or windows
showing other buffers.  Should be called from an `org-inside' buffer."
  (setq org-inside--states
	(cl-loop with buf = (current-buffer)
		 for s in org-inside--states
		 for win = (ois/window s)
		 if (and (not all) (window-live-p win)
			 (eq (window-buffer win) buf) org-inside-mode)
		 collect s else do (org-inside--reset-state s))))

(defun org-inside--make-overlay (win &optional unhide secondary-p)
  "Return a new front-advancing overlay specific to window WIN.
If SECONDARY-P is non-nil, create a secondary overlay without
properties.  Otherwise, assign `cursor-sensor-functions' or
`modification-hooks', and, if UNHIDE is non-nil, the `invisible'
property."
  (let* ((buf (window-buffer win))
         (ov (make-overlay 1 1 buf t)))
    (overlay-put ov 'window win)
    (unless secondary-p
      (overlay-put ov 'cursor-sensor-functions '(org-inside--sensor))
      (overlay-put ov 'modification-hooks '(org-inside--overlay-modification))
      ;; For auto-unhiding, we set the invisible property to something
      ;; guaranteed not to be on the `buffer-invisibility-spec'.
      (when unhide (overlay-put ov 'invisible 'org-inside--not-hidden)))
    ov))

(defun org-inside--state-for-window (win &optional unhide secondary-p)
  "Return the state for window WIN, creating and caching it if necessary.
UNHIDE is the hide status; see the custom variable
`org-inside-appearance'.  If UNHIDE is non-nil, invisibility will be
configured on the primary overlay to unhide the underlying hidden text.
If SECONDARY-P is non-nil, a valid secondary overlay will be included in
the returned state.  Otherwise, the secondary may be nil or invalid.
Note that this function does not set the `face' property."
  (let ((state (cl-find win org-inside--states :key #'ois/window)))
    (if state ; a saved state may lack a secondary overlay
        (when (and secondary-p (not (overlayp (ois/ov2 state))))
          (setf (ois/ov2 state) (org-inside--make-overlay nil win 'secondary)))
      (setq state (ois/create
                   :window win
                   :ov (org-inside--make-overlay win unhide)
                   :ov2 (and secondary-p
                             (org-inside--make-overlay win nil 'secondary))))
      (cl-callf2 push state org-inside--states))
    state))

(defun org-inside--state (win unhide face &optional with-secondary-p)
  "Return an appropriate `org-inside' state for window WIN.
UNHIDE and FACE are the invisibility status and text face; see the
custom variable `org-inside-appearance'.  Returns a structure of type
`org-inside-state'.

If FACE and WITH-SECONDARY-P are non-nil, ensure the SECONDARY overlay
is valid and contains the `face' property set to FACE (and the PRIMARY
contains no `face' property).  If FACE-OVERLAY-P is nil, the SECONDARY
overlay may be invalid."
  (let* ((sec-p (and face with-secondary-p (>= emacs-major-version 31)))
         (state (org-inside--state-for-window win unhide sec-p)))
    (overlay-put (ois/ov state) 'face (unless sec-p face)) ; always update face
    (when sec-p (overlay-put (ois/ov2 state) 'face face))
    state))

(defun org-inside--set-appearance (win &optional beg end beg2 end2)
  "Set appearance and hidden state for hidden-contents entities.
The region is from BEG to END in window WIN's buffer.  If BEG or END are
nil, point is considered to be outside the text and the prior
\"outside\" appearance is restored.  If WIN is nil, the selected window
is used.

If BEG2 and END2 are non-nil, they represent a sub-range fully within
BEG and END containing point, over which to apply face modifications
using a secondary overlay.  This allows face modification to cover a
smaller region within the main BEG..END range, for indicating nested
entities.

Note that if the `cursor-type' is configured to change inside but the
`window-cursor-type' is currently nil (i.e. the cursor is hidden), the
cursor is left hidden, and the window parameter `pending-cursor-type' is
set instead.  Tools can consult this window parameter to restore the
cursor type."
  (cl-destructuring-bind (&key cursor face unhide) org-inside-appearance
    (let* ((inside-p (and beg end))
           (win (or win (selected-window)))
           (state (org-inside--state win unhide face (not (null beg2))))
           (ov (ois/ov state))
           (showing-p (overlay-get ov 'invisible)) ; non-nil = unhidden!
           (ov2 (ois/ov2 state)))
      ;; more natural movement moving outside when hidden text is visible
      (unless (or (not showing-p) inside-p)
        (setq disable-point-adjustment t))
      ;; User may have toggled hiding in a saved state; reset it
      (when (not inside-p)
        (overlay-put ov 'invisible (and unhide 'org-inside--not-hidden)))
      ;; Update the cursor type
      (when cursor
        (let ((cursor (if inside-p cursor
                        (or (ois/saved-cursor-type state) t)))
              (win-cursor-type (window-cursor-type win)))
          (if (eq win-cursor-type nil)
	      ;; Do not override a hidden (nil) cursor; set it pending instead
              (set-window-parameter win 'pending-cursor-type cursor)
            (unless (eq cursor win-cursor-type) ; already set?
              (when inside-p                    ; save old type
	        (setf (ois/saved-cursor-type state) win-cursor-type))
              (set-window-cursor-type win cursor)))))
      ;; Move the overlays into place, or remove them.  We do this
      ;; when returning to the run-loop to avoid the cursor-sensor
      ;; race for point adjustment.  This can happen since our overlay
      ;; can unhide the very text point adjustment is skipping.
      (run-at-time 0 nil
                   (lambda (buf)
                     (with-current-buffer buf
                       (if inside-p (move-overlay ov beg end)
                         (delete-overlay ov))
                       (if beg2
                           (move-overlay ov2 beg2 end2)
                         (when (overlayp ov2) (delete-overlay ov2)))))
                   (current-buffer)))))

(defun org-inside--visible-region (elem)
  "Return the visible region of entity ELEM.
Returned region is a cons (BEG . END), or nil if the region does not
begin with a character marked invisible."
  (let  ((beg (org-element-begin elem))
         (end (- (org-element-end elem) (org-element-post-blank elem))))
    (when (get-text-property beg 'invisible)
      (setq beg (next-single-property-change beg 'invisible nil end)
            end (next-single-property-change beg 'invisible nil end))
      (cons beg end))))

(defun org-inside--sensor (win _pos type)
  "Handle cursor appearance and unhiding inside entities with hidden contents.
To be set via the `cursor-sensor-functions' property, as well as the
overlay in each `org-inside-state' .  WIN POS, and TYPE are the window,
former position, and cursor movement type."
  (unless (minibuffer-window-active-p win)
    (cond
     ((or (eq type 'entered)   ; called from in-text cursor-sensor
          (and (eq type 'moved) (plist-get org-inside-appearance :face)))
      (when-let* ((elems (org-inside--elems-at-point)) ; ordered inner->outer
                  (outer-elem (car (last elems))))
        (let ((beg (org-element-begin outer-elem))
              (end (- (org-element-end outer-elem)
                      (org-element-post-blank outer-elem)))
              beg2 end2)
          (when (and (> (length elems) 1)        ; nested entities
                     (>= emacs-major-version 31) ; needed for `moved'
                     (plist-get org-inside-appearance :face))
            (pcase-let ((`(,b . ,e) (org-inside--visible-region (car elems))))
              (if (<= b (point) e) (setq beg2 b end2 e)
                ;; We are within a relevant inner org-element, but
                ;; outside its visible region.  Use the level above,
                ;; if any.
                (when (> (length elems) 2)
                  (pcase-setq `(,beg2 . ,end2)
                              (org-inside--visible-region (cadr elems)))))))
          (org-inside--set-appearance win beg end beg2 end2))))
     ((eq type 'left) ; called from the primary overlay's override cursor-sensor
      (org-inside--set-appearance win)))))

(defun org-inside--buffer-changed (win)
  "Handle `org-inside' buffers appearing or disappearing from window WIN."
  (when org-inside--states
    (org-inside--trim-states)
    (if org-inside-mode
        (org-inside--sensor win nil
                            (if (org-inside--elems-at-point) 'entered 'left))
      (kill-local-variable 'org-inside--states))))

(defun org-inside--frame-changed (frame)
  "Handle `org-inside' buffers disappearing for all windows on FRAME.
Not needed on v31+, as the buffer-local value of
`window-buffer-change-functions' is called for buffers both appearing
and disappearing there."
  (walk-windows
   (lambda (win)
     (with-current-buffer (window-old-buffer win)
       (org-inside--buffer-changed win))
     nil frame)))

(defun org-inside--add-properties (type _beg _end visible-beg visible-end)
  "Add text properties to invisible text for org-inside functionality.
TYPE is the type of text being hidden.  BEG, END, VISIBLE-BEG,
VISIBLE-END are the buffer positions of the affected text and its
visible portion.  To be set on `org-hidden-text-functions'."
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
    (org-inside--buffer-changed w))
  (org-inside--setup-hidden-contents-types))

(defun org-inside--teardown ()
  "Tear down `org-inside-mode' in buffer."
  (org-inside--trim-states 'all)
  (cursor-sensor-mode -1)
  (setq-local org-extra-unfontify-properties
              (delq 'cursor-sensor-functions org-extra-unfontify-properties))
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-inside-toggle-hidden t)
  (remove-hook 'org-hidden-text-functions #'org-inside--add-properties t)
  (remove-hook 'window-buffer-change-functions #'org-inside--buffer-changed t))

(defun org-inside--reset-all ()
  "Reset `org-inside' in all windows showing org-inside buffers."
  (walk-windows
   (lambda (win)
     (with-selected-window win
       (when org-inside-mode
         (org-inside--buffer-changed win))))
   nil t))

(defun org-inside-toggle-hidden ()
  "Toggle visibility of hidden text for outermost entity at point (if any).
Operates only when inside an entity wrapped by hidden text.  Text will
be re-hidden when point leaves the entity.  See `org-inside-appearance'
to enable automatic unhiding or configure other appearance settings.
Returns non-nil if the visibility was toggled, making it suitable for
inclusion on `org-ctrl-c-ctrl-c-hook'."
  (interactive)
  (when (org-inside--elems-at-point)
    (cl-destructuring-bind (&key unhide face &allow-other-keys)
        org-inside-appearance
      (let* ((state (org-inside--state (selected-window) unhide face))
             (ov (ois/ov state))
             (inv (overlay-get ov 'invisible)))
        (overlay-put ov 'invisible (if inv nil 'org-inside--not-hidden))
        t))))

;;;###autoload
(define-minor-mode org-inside-mode
  "Change appearance when point is inside an entity wrapped by hidden text.
The cursor type and/or text face can be altered when point is inside the
hidden region.  \"Inside\" means any characters entered at that point
will appear with the visible text.  For example, entering a character
when \"inside\" underlined text would make the new character underlined
as well.  I.e., for position `[x]':

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

;; Starting in v31, buffer-local change functions are run in
;; windows a buffer left as well those it entered.
(unless (>= emacs-major-version 31)
  (add-hook 'window-buffer-change-functions #'org-inside--frame-changed))

(provide 'org-inside)
;;; org-inside.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ois/" . "org-inside-state-"))
;; End:
