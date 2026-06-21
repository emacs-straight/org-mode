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

(defun org-inside--overlays (win face unhide &optional face-overlay-p)
  "Return appropriately styled overlays for window WIN.
FACE and UNHIDE are the text face and invisibility status; see the
custom variable `org-inside-appearance'.  Returns a cons cell of two
overlays:

  (PRIMARY . SECONDARY)

If FACE-OVERLAY-P is non-nil, ensure the SECONDARY overlay is valid and
contains the FACE property (and PRIMARY contains no `face' property).
If FACE-OVERLAY-P is nil, SECONDARY may be nil."
  (let* ((ovs (window-parameter win 'org-inside-overlays))
         (ov (car ovs))
         (do-secondary (and face face-overlay-p (>= emacs-major-version 31)))
         ov2)
    (if (and ovs (overlayp ov))
        (when do-secondary
          (setq ov2 (cdr ovs))
          (unless (and ov2 (overlayp ov2))
            (setq ov2 (make-overlay 1 1 (window-buffer win) t))
            (overlay-put ov2 'window win)
            (setcdr ovs ov2)))
      ;; (Re)create both overlays
      (setq ov (make-overlay 1 1 (window-buffer win) t))
      (overlay-put ov 'window win)
      (overlay-put ov 'cursor-sensor-functions '(org-inside--sensor))
      (overlay-put ov 'modification-hooks '(org-inside--overlay-modification))
      ;; For auto-unhiding, we set the invisible property to something
      ;; guaranteed not to be on the `buffer-invisibility-spec'.
      (when unhide (overlay-put ov 'invisible 'org-inside--not-hidden))
      (when do-secondary
        (setq ov2 (make-overlay 1 1 (window-buffer win) t))
        (overlay-put ov2 'window win))
      (setq ovs (cons ov ov2))
      (set-window-parameter win 'org-inside-overlays ovs))
    (when ov2 (overlay-put ov2 'face (and do-secondary face)))
    (overlay-put ov 'face (unless do-secondary face))
    ovs))

(defun org-inside--set-appearance (win &optional beg end beg2 end2)
  "Set appearance and hide state for hidden-contents entities.
The region is from BEG to END in the window WIN's buffer.  If BEG or END
are nil, point is considered to be outside the text and the prior
\"outside\" appearance is restored.  If BEG2 and END2 are non-nil, they
are positions fully within BEG and END and containing point, over which
to apply face modifications (if requested).  This allows any face
modification to cover a smaller region within the main BEG..END range,
for nested entities.

Note that if the `cursor-type' is configured to change inside (see
`org-inside-appearance') but the `window-cursor-type' is currently
nil (i.e. the cursor is hidden), the cursor is left hidden, and the
window parameter `pending-cursor-type' is set instead.  Other tools can
consult this window parameter to restore the cursor type."
  (cl-destructuring-bind ( &key cursor face unhide) org-inside-appearance
    (let* ((inside-p (and beg end))
           (ovs (org-inside--overlays win face unhide (and beg2 'face-overlay)))
           (showing-p (overlay-get (car ovs) 'invisible))) ; non-nil = unhidden!
      ;; We move the overlay when returning to the run-loop to avoid
      ;; the cursor-sensor race for point adjustment, since our
      ;; overlay unhides text which point adjustment can skip.  As
      ;; well, since both text and overlay implement
      ;; cursor-sensor-functions, this avoids a similar race as to
      ;; which applies.
      (run-at-time 0 nil (lambda (buf)
                           (with-current-buffer buf
                             (when (overlayp (car ovs))
                               (if inside-p (move-overlay (car ovs) beg end)
                                 (delete-overlay (car ovs))))
                             (when (overlayp (cdr ovs))
                               (if (and face beg2)
                                   (move-overlay (cdr ovs) beg2 end2)
                                 (delete-overlay (cdr ovs))))))
                   (current-buffer))
      ;; more natural movement moving out when hidden text is visible
      (unless (or (not showing-p) inside-p)
        (setq disable-point-adjustment t))
      ;; User may have toggled hiding; re-set
      (when (not inside-p)
        (overlay-put (car ovs) 'invisible (and unhide 'org-inside--not-hidden)))
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
To be set via the `cursor-sensor-functions' property on hidden-contents
text, as well as the overlay returned by `org-inside--overlay' .  WIN
POS, and TYPE are the window, former position, and cursor movement type."
  (unless (minibuffer-window-active-p win)
   (cond
    ((or (eq type 'entered) ; called from the in-text cursor-sensor
         (and (plist-get org-inside-appearance :face) (eq type 'moved)))
     (when-let* ((elems (org-inside--elems-at-point))
                 (top-elem (car (last elems))))
       (let ((beg (org-element-begin top-elem))
             (end (- (org-element-end top-elem)
                     (org-element-post-blank top-elem)))
             inner-elem beg2 end2)
         (when (> (length elems) 1)     ; nested elements
           (setq inner-elem (car elems)
                 beg2 (org-element-begin inner-elem)
                 end2 (- (org-element-end inner-elem)
                         (org-element-post-blank inner-elem)))
           ;; Locate always-visible portion of inner element
           (setq beg2 (next-single-property-change beg2 'invisible nil end2)
                 end2 (next-single-property-change beg2 'invisible nil end2))
           (unless (<= beg2 (point) end2) (setq beg2 nil end2 nil)))
         (org-inside--set-appearance win beg end beg2 end2))))
    ((eq type 'left)         ; called from the overlay's cursor-sensor
     (org-inside--set-appearance win)))))

(defsubst org-inside--restore-cursor (win)
  "Restore old cursor in WIN (if any).
If the current window cursor type is nil (i.e. the cursor is hidden), no
change is made."
  (when-let* ((old-type (or (window-parameter win 'org-inside-old-cursor)
                            (window-parameter win 'pending-cursor-type)))
              (type (window-cursor-type win)))
    (set-window-cursor-type win old-type)
    (set-window-parameter win 'org-inside-old-cursor nil)))

(defsubst org-inside--clear-overlays (win)
  "Clear the `org-inside' overlay from window WIN."
  (when-let* ((ovs (window-parameter win 'org-inside-overlays)))
    (when (overlayp (car ovs)) (delete-overlay (car ovs)))
    (when (overlayp (cdr ovs)) (delete-overlay (cdr ovs)))
    (set-window-parameter win 'org-inside-overlays nil)))

(defsubst org-inside--in-hidden-contents-text (&optional pos)
  "Return non-nil if inside hidden contents text.
If POS is nil, use point."
  (and-let* ((csf (get-text-property (or pos (point))
                                     'cursor-sensor-functions))
             (_ (memq 'org-inside--sensor csf)))))

(defvar org-inside-mode)
(defun org-inside--buffer-changed (win)
  "Handle `org-inside' buffers appearing in window WIN."
  (with-current-buffer (window-buffer win)
    (when org-inside-mode
      (if (org-inside--in-hidden-contents-text)
          (org-inside--sensor win nil 'entered)
        (org-inside--sensor win nil 'left)))))

(defun org-inside--frame-changed (frame)
  "Handle window buffer change for all windows on FRAME."
  (walk-windows
   (lambda (win)
     (unless (buffer-local-value 'org-inside-mode (window-buffer win))
       (org-inside--clear-overlays win)
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
  (add-hook 'org-fold-reveal-start-hook #'org-inside-toggle-hidden nil t)
  (font-lock-flush) ;; does not call sensor functions
  (dolist (w (get-buffer-window-list nil nil t))
    (org-inside--buffer-changed w))
  (org-inside--setup-hidden-contents-types))

(defun org-inside--teardown ()
  "Tear down `org-inside-mode' in buffer."
  (dolist (w (get-buffer-window-list nil nil t))
    (org-inside--restore-cursor w)
    (org-inside--clear-overlays w))
  (cursor-sensor-mode -1)
  (setq-local org-extra-unfontify-properties
              (delq 'cursor-sensor-functions org-extra-unfontify-properties))
  (remove-hook 'org-fold-reveal-start-hook #'org-inside-toggle-hidden t)
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-inside-toggle-hidden t)
  (remove-hook 'org-hidden-text-functions #'org-inside--add-properties t)
  (remove-hook 'window-buffer-change-functions #'org-inside--buffer-changed t))

(defun org-inside--reset-all ()
  "Reset org-inside in all `org-inside' buffers."
  (walk-windows
   (lambda (win)
     (when (window-parameter win 'org-inside-overlays)
       (with-selected-window win
         (org-inside--clear-overlays win)
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
  (and-let* ((ovs (window-parameter nil 'org-inside-overlays))
             (ov (car ovs))
             (_ (and (overlayp ov) (overlay-buffer ov))))
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
