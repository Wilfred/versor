;;; versor.el -- versatile cursor
;;; Time-stamp: <2004-05-24 09:39:58 john>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;; 
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(provide 'versor)
(require 'tempo)
(require 'cl)
(require 'versor-dimensions)
(require 'versor-base-moves)
(require 'versor-commands)

(defgroup versor nil
  "Switch cursor keys between different sorts of movement."
  :version "0.1"
  :group 'editing
  :prefix "versor-")

(defcustom versor:level-wrap t
  "*Whether to wrap the level changes"
  :group 'versor
  :type 'boolean)

(defcustom versor:meta-level-wrap t
  "*Whether to wrap the meta-level changes"
  :group 'versor
  :type 'boolean)

(defcustom versor-change-cursor-color t
  "*Whether to use the cursor color to indicate the level."
  :group 'versor
  :type 'boolean)

(defcustom versor-item-attribute nil
  "*An attribute to use to indicate the current item.
This is looked up in the current item, to get the value to set it to.
You can only use this from Emacs 21 onwards.")

(defcustom versor:try-to-display-whole-item t
  "*Whether to try to display the whole item after each movement."
  :group 'versor
  :type 'boolean)

(defcustom versor:reversible (not (eq window-system 'x))
  "*Whether we allow reversing.
This is useful if you cannot use \"shift-next\" for \"previous\".
These seem to work OK on X but not on Windows; not sure about
other platforms/terminals yet."
  :group 'versor
  :type 'boolean)

(defcustom phrase-end "[,;:] *"
  "*Regexp describing the end of a phrase."
  :group 'versor
  :type 'string)

;;;;;;;;;;;;;;;
;;;; hooks ;;;;
;;;;;;;;;;;;;;;

(defvar versor:start-hooks nil
  "*Hooks for versor:start.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor:prev-hooks nil
  "*Hooks for versor:prev.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor:next-hooks nil
  "*Hooks for versor:next.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor:end-hooks nil
  "*Hooks for versor:end.
If one of these returns non-nil, it is taken as having done the action.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autoloads for structured text ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'nested-blocks-backward "nested-blocks"
  "Move back over a nested block. Like backward-sexp but more general."
  t)

(autoload 'nested-blocks-forward "nested-blocks"
  "Move forward over a nested block. Like forward-sexp but more general."
  t)

(autoload 'nested-blocks-enter "nested-blocks"
  "Into the next nested block forwards."
  t)

(autoload 'nested-blocks-leave "nested-blocks"
  "Move forwards out of current nested block."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autoloads for language-guided navigation ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'navigate-head "languide"
  "Navigate to the head of the relevant statement."
  t)

(autoload 'statement-navigate-parts-previous "languide"
  "Navigate to the previous part of the statement."
  t)

(autoload 'statement-navigate-parts-next "languide"
  "Navigate to the next part of the statement."
  t)

(autoload 'navigate-body "languide"
  "Navigate to the body of the relevant statement."
  t)

(autoload 'latest-statement-navigation-end "languide"
  "Return the end of the latest statement navigation result.
Optional argument for compatibility with other things that get the versor
package to the end of an item."
  t)

;;;;;;;;;;;;;;;
;;;; Setup ;;;;
;;;;;;;;;;;;;;;

;; To use versor:
;;   add the directory containing this elisp file, and its companions, to your load-path
;;   load this file (or autoload versor:setup)
;;   call versor-setup
;; If you want to use navigation by statements, you will need languide too.

(defvar versor:insertion-placement-keymap (make-sparse-keymap "Versor insert place")
  "Keymap for reading what place to do insertion at.
This is for choosing before, after, around or inside.")

(fset 'versor:insertion-placement-keymap versor:insertion-placement-keymap)

(defvar versor:insertion-kind-alist nil
  "Alist for reading what kind of insertion to do.
Almost a keymap, but the functions it contains are not commands.")

(defun versor:setup (&rest keysets)
  "Set up the versor (versatile cursor) key system.

This rebinds the cursor (arrow) keys in such a way that they can move
the cursor in other ways than just cartesian (up/down,
left/right). Instead, they take a pair of adjacent dimensions in a
series of dimensions (such as characters/lines or lines/pages).

Several such series (meta-dimensions) of dimensions are available. The
vertical arrow keys are always for the more significant of the two
dimensions, just as they are in the normal key bindings.

Meta-arrows change the dimensions. M-left and M-right shift the pair
of dimensions within the current series, and M-up and M-down change
which series you are in. The cursor colour indicates which dimension
is selected, and the current meta-dimension and dimension are shown
in the mode line.

The current item is highlighted after each versor move. The
highlighting is removed at the next command, so versor can co-operate
unobtrusively with traditional character-based editing.

DEL deletes the current item.

The meta-dimensions and dimensions defined by default are:
  cartesian (char, line, page)
  structural (char, expression, expression depth, defun)
  natural language oriented (char, word, phrase, sentence, paragraph)
  tabular text (HTML table rows and cells)
  programming language oriented (expression, part of statement, statement)

The arguments can be any (combination) of
  'arrows        -- for the main cursor keys
  'arrows-misc   -- for insert, delete etc
  'keypad        -- for the keypad cursor keys
  'keypad-misc   -- for keypad insert, delete etc
to select which keys are set up to do versor commands."
  (interactive)

  (if (null keysets) (setq keysets '(arrows)))

  (when (memq 'arrows keysets)
    (global-set-key [ left ]    'versor:prev)
    (global-set-key [ right ]   'versor:next)
    (global-set-key [ M-left ]  'versor:out)
    (global-set-key [ M-right ] 'versor:in)
    (global-set-key [ C-left ]  'versor-extend-item-backwards)
    (global-set-key [ C-right ] 'versor-extend-item-forwards)
    (global-set-key [ home ]    'versor:start)
    (global-set-key [ end ]     'versor:end)
  
    (global-set-key [ up ]      'versor:over-prev)
    (global-set-key [ down ]    'versor:over-next)
    (global-set-key [ M-up ]    'versor:prev-meta-level)
    (global-set-key [ M-down ]  'versor:next-meta-level)
    (global-set-key [ C-up ]    'versor:over-start)
    (global-set-key [ C-down ]  'versor:over-end)
    (global-set-key [ C-home ]  'versor-start-of-item)
    (global-set-key [ C-end ]   'versor-end-of-item)
    )

  (when (memq 'arrows-misc keysets)
    (global-set-key [ prior ]   'versor:over-over-prev)
    (global-set-key [ next ]    'versor:over-over-next)
    (global-set-key [ C-prior ] 'versor:over-over-start)
    (global-set-key [ C-next ]  'versor:over-over-end)
    
    (define-key (current-global-map) [ insert ]   'versor:insertion-placement-keymap)
    (global-set-key [ delete ]   'versor:kill)
    (global-set-key [ M-delete ] 'versor:copy)

    (define-key versor:insertion-placement-keymap [ left ]  'versor:insert-before)
    (define-key versor:insertion-placement-keymap [ right ] 'versor:insert-after)
    (define-key versor:insertion-placement-keymap [ up ]    'versor:insert-around)
    (define-key versor:insertion-placement-keymap [ down ]  'versor:insert-within)
    )

  (when (memq 'keypad keysets)
    (global-set-key [ kp-left ]    'versor:prev)
    (global-set-key [ kp-right ]   'versor:next)
    (global-set-key [ M-kp-left ]  'versor:out)
    (global-set-key [ M-kp-right ] 'versor:in)
    (global-set-key [ C-kp-left ]  'versor:start)
    (global-set-key [ C-kp-right ] 'versor:end)
    (global-set-key [ kp-home ]    'versor:start)
    (global-set-key [ kp-end ]     'versor:end)
    (global-set-key [ C-kp-home ]  'versor-start-of-item)
    (global-set-key [ C-kp-end ]   'versor-end-of-item)

  
    (global-set-key [ kp-up ]      'versor:over-prev)
    (global-set-key [ kp-down ]    'versor:over-next)
    (global-set-key [ M-kp-up ]    'versor:prev-meta-level)
    (global-set-key [ M-kp-down ]  'versor:next-meta-level)
    (global-set-key [ C-kp-up ]    'versor:over-start)
    (global-set-key [ C-kp-down ]  'versor:over-end)

    (global-set-key [ kp-prior ]   'versor:over-over-prev)
    (global-set-key [ kp-next ]    'versor:over-over-next)
    (global-set-key [ C-kp-prior ] 'versor:over-over-start)
    (global-set-key [ C-kp-next ]  'versor:over-over-end))

  (when (memq 'keypad-misc keysets)
    (global-set-key [ kp-enter ]  'versor:copy)
    (define-key (current-global-map)
                    [ kp-insert ] 'versor:insertion-placement-keymap)
    (global-set-key [ kp-delete ] 'versor:kill)
    (global-set-key [ kp-add ]    'other-window)

    (define-key versor:insertion-placement-keymap [ kp-left ]  'versor:insert-before)
    (define-key versor:insertion-placement-keymap [ kp-right ] 'versor:insert-after)
    (define-key versor:insertion-placement-keymap [ kp-up ]    'versor:insert-around)
    (define-key versor:insertion-placement-keymap [ kp-down ]  'versor:insert-within)
    )
  
  (unless (memq 'versor:current-level-name global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(""		; needed to stop fancy control actions
					; in the rest of this list
		    versor:mode-line-begin-string
		    versor:current-meta-level-name ":"
		    versor:current-level-name
		    versor:mode-line-end-string))))
  
  (versor:set-status-display))

(defvar versor:item-face (make-face 'versor-item)
  "Face to use for versor items")

(defvar versor:use-face-attributes
  (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 21))
  "*Whether to use face attributes, as provided from Emacs 21 onwards.")

(defvar versor:multi-line-level-display (and (boundp 'emacs-major-version)
					     (>= emacs-major-version 21))
  "*Whether to use multi-line indication of the current meta-level and level.")

(when versor:use-face-attributes
  (set-face-attribute 'versor-item nil :inherit 'region))

(defun versor:set-status-display (&optional one of-these explicit)
  "Indicate the state of the versor system."
  (setq versor:current-level-name (first (versor:current-level))
	versor:current-meta-level-name (aref (versor:current-meta-level) 0))
  (if (and versor:multi-line-level-display explicit)
      (versor:display-dimensions-2d)
    (if one
	(if of-these
	    (versor:display-highlighted-choice one of-these)
	  (if (stringp one)
	      (message one)))
      (message (first (versor:current-level)))))
  (if versor:reversible
      (setq versor:mode-line-begin-string (if versor:reversed " <==" " <")
	    versor:mode-line-end-string (if versor:reversed ">" "==>"))
    (setq versor:mode-line-begin-string " <"
	  versor:mode-line-end-string ">"))
  (force-mode-line-update t)
  (when versor-change-cursor-color 
    (set-cursor-color (versor:action (versor:current-level) 'color)))
  (when (and versor-item-attribute (fboundp 'set-face-attribute))
    (set-face-attribute 'versor-item nil
			versor-item-attribute
			(versor:action (versor:current-level)
				       versor-item-attribute)))
  (let ((old-pair (assoc major-mode versor:mode-current-levels)))
    (if (null old-pair)
	(push (cons major-mode (cons versor:meta-level versor:level))
	      versor:mode-current-levels)
      (rplaca (cdr old-pair) versor:meta-level)
      (rplacd (cdr old-pair) versor:level))))

(defun versor:highlighted-string (string)
  "Return a highlighted version of STRING."
  (if versor:use-face-attributes
      (let ((strong (copy-sequence string)))
	(put-text-property 0 (length string)
			   'face 'versor-item
			   strong)
	strong)
    (format "[%s]" string)))

(defun versor:display-highlighted-choice (one of-these-choices)
  "Display, with ONE highlighted, the members of OF-THESE-CHOICES"
  (let* ((msg (mapconcat
	       (lambda (string)
		 (if (string= string one)
		     (versor:highlighted-string string)
		   string))
	       of-these-choices
	       ", ")))
    (message msg)))

(defvar versor:max-meta-name-length nil
  "The length of the longest meta-level name.
Used for display purposes, and cached here.")

(defun versor:display-dimensions-2d ()
  "Indicate the current meta-level and level, in a multi-line message."
  (interactive)
  (unless versor:max-meta-name-length
    (setq versor:max-meta-name-length
	  (apply 'max
		 (mapcar 'length
			 (mapcar 'car
				 (versor:meta-level-names))))))
  (message
   (let ((meta-levels-name-format-string (format "%% %ds" versor:max-meta-name-length)))
     (mapconcat
      'identity
      (let ((meta (1- (length moves-moves)))
	    (formats (reverse (versor:all-names-grid-formats)))
	    (result nil))
	(while (>= meta 1)
	  (let* ((meta-data (aref moves-moves meta))
		 (meta-name (aref meta-data 0))
		 (inner-result nil)
		 (row-formats formats)
		 (level 1)
		 (n-level (length meta-data)))
	    (while row-formats
	      (let* ((level-name-raw 
		      (if (< level n-level)
			  (first (aref meta-data level))
			""))
		     (level-name (format (car row-formats) level-name-raw)))
		(push
		 (if (and (= meta versor:meta-level)
			  (= level versor:level))
		     (versor:highlighted-string level-name)
		   level-name)
		 inner-result)
		(setq row-formats (cdr row-formats))
		(incf level)))
	    (push
	     (concat
	      (format meta-levels-name-format-string
		      (if (= meta versor:meta-level)
			  (versor:highlighted-string meta-name)
			meta-name))
	      ": "
	      (mapconcat 'identity inner-result " "))
	     result)
	    (decf meta)))
	result)
      "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; underlining current item ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar versor-items (list 'not-yet-set)
  ;; what was this old value? see if it works ok without it!
  ;; (list (cons (cons nil nil) nil))
  "The current versor items.
There is a separate value of this variable for each buffer.
Each item is represented by an overlay.
There is always at least one item in this list, to avoid churning
overlays; if there is no current item, the top item has nil for its
buffer.")

(defvar versor:latest-items nil
  "The items list as it was at the start of the current command,
but converted from overlays to conses of start . end, as the overlays
get cancelled on the pre-command-hook versor-de-indicate-current-item,
which we use to make sure we go away quietly when the user wants to type
other emacs commands.")

(mapcar 'make-variable-buffer-local
	'(versor-items
	  versor:latest-items))

(defun versor:set-current-item (start end)
  "Set the START and END of the current item, and get rid of any extra parts."
  (mapcar 'delete-overlay (cdr versor-items))
  (rplacd versor-items nil)
  (let ((item (and versor-items
		   (car versor-items))))
    (if (and (overlayp item) (overlay-buffer item))
	(move-overlay item start end (current-buffer))
      (make-versor-overlay start end))))

(defun versor:current-item-valid ()
  "Return whether there is a valid current item."
  (let ((item (car versor-items)))
    (and (overlayp item)
	 (overlay-buffer item))))

(defun versor:get-current-item ()
  "Return (start . end) for the current item.
If there are multiple items, return only the first one."
  (cond
   ((versor:current-item-valid)
    (let ((olay (car versor-items)))
      (cons (overlay-start olay)
	    (overlay-end olay))))
   ((versor:valid-item-list-p versor:latest-items)
    (car versor:latest-items))
   (t
    (versor:make-item-from-region))))

(defun versor:get-current-items ()
  "Return the current items."
  (cond 
   ((versor:current-item-valid)
    versor-items)
   ((versor:valid-item-list-p versor:latest-items)
    versor:latest-items)
   (t
    (let ((pair (versor:make-item-from-region)))
      (make-versor-overlay (car pair) (cdr pair))
      versor-items))))

(defun versor-overlay-start (olay)
  "Return the start of OLAY.
OLAY may be an overlay, or a cons used to preserve data from a destroyed overlay."
  (if (consp olay)
      (car olay)
    (overlay-start olay)))

(defun versor-overlay-end (olay)
  "Return the end of OLAY.
OLAY may be an overlay, or a cons used to preserve data from a destroyed overlay."
  (if (consp olay)
      (cdr olay)
    (overlay-end olay)))

(defun versor:display-item-list (label il)
  "With LABEL, display IL.
Meant for debugging versor itself."
  (message label)
  (mapcar (lambda (item)
	    (let ((start (versor-overlay-start item))
		  (end (versor-overlay-end item)))
	      (if (< (- end start) 16)
		  (message "  %d..%d: %s" start end
			   (buffer-substring start end))
	      (message "  %d..%d: %s..%s" start end
		       (buffer-substring start (+ start 8))
		       (buffer-substring (- end 8) end)))))
	  il))

(defun versor:make-item-from-region ()
  "Make a versor item from the current region.
Meant to be used by things that require an item, when there is none."
  (let* ((end (progn (funcall (or (versor:get-action 'end-of-item)
				  (versor:get-action 'next))
			      1)
		     (point)))
	 (start (progn (funcall (versor:get-action 'previous) 1)
		       (point))))
    (cons start end)))

(defun versor:valid-item-list-p (a)
  "Return whether A is a valid item list."
  (and (consp a)
       (let ((cara (car a)))
	 (or (and (consp cara)
		  (integerp (car cara))
		  (integerp (cdr cara)))
	     (and (overlayp cara)
		  (bufferp (overlay-buffer cara)))))))

(defun versor-item-overlay ()
  "The (primary) item indication overlay for this buffer."
  (car versor-items))

(defun make-versor-overlay (start end)
  "Make a versor overlay at START END.
If there is already a versor overlay for this buffer, reuse that.
You should normally call versor:set-current-item rather than this."
  (unless (overlayp (versor-item-overlay))
    (let ((overlay (make-overlay start end (current-buffer))))
      (setq versor-items (list overlay))
      (overlay-put overlay 'face
		   (if versor:use-face-attributes
		       'versor-item
		     'region)
		   )))
  (move-overlay (versor-item-overlay) start end (current-buffer)))

(defun versor-extra-overlay (start end)
  "Make an extra versor overlay between START and END."
  (let ((overlay (make-overlay start end (current-buffer))))
    (overlay-put overlay 'face
		 (if versor:use-face-attributes
		     'versor-item
		   'region))
    (rplacd versor-items
	    (cons overlay
		  (cdr versor-items)))))

(defun delete-versor-overlay ()
  "Delete the versor overlay for this buffer."
  ;; in fact, we just disconnect it from the buffer,
  ;; to avoid having to keep creating and destroying overlays
  (let ((overlay (versor-item-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  ;; get rid of any extras
  (when versor-items
    (mapcar 'delete-overlay (cdr versor-items))
    (rplacd versor-items nil)))

(defun versor-clear-current-item-indication ()
  "Intended to go at the start of each versor motion command.
It clears variables looked at by versor-indicate-current-item, which sets them
up for itself only if the command didn't set them for it."
  (setq versor-indication-known-valid nil)
  (delete-versor-overlay))

(defun versor-indicate-current-item ()
  "Make the current item distinctly visible.
This is intended to be called at the end of all versor commands.
See also the complementary function versor-de-indicate-current-item,
which goes on the pre-command-hook up, to make sure that versor gets out of the way
of ordinarily Emacs commands."

  (condition-case error-var
      (progn

	;; the command we have just run may have set these for us,
	;; if it knows in such a way that it can tell us much faster
	;; than we could work out for ourselves; they are cleared at
	;; the start of each command by
	;; versor-clear-current-item-indication, which, like us, is
	;; called by the as-versor-command macro

	(unless (versor:current-item-valid)
	  (versor:set-current-item (point) (versor-end-of-item-position)))

	(when versor:try-to-display-whole-item
	  (let* ((item (versor:get-current-item))
		 (end (cdr item)))
	    ;; try to get the whole item on-screen
	    (when (> end (window-end))
	      (let* ((start (car item))
		     (lines-needed (count-lines start end))
		     (lines-available (- (window-height (selected-window)) 2)))
		(if (<= lines-needed lines-available)
		    (recenter (/ (- lines-available lines-needed) 2))
		  (recenter 0)
		  (message "%d more lines of this item would not fit on screen" (- lines-needed lines-available 1)))))))

	;; re-do this because it somehow gets taken off from time to time
	(add-hook 'pre-command-hook 'versor-de-indicate-current-item))
    (error
     (progn
       ;; (message "Caught error %S in item indication" error-var)
       ;; (with-output-to-temp-buffer "*Backtrace for item indication*" (backtrace))
       (versor-de-indicate-current-item)
       ))))

(defun versor-de-indicate-current-item ()
  "Remove the current item marking.
Intended to go on pre-command-hook, to make sure that versor gets out
of the way of ordinarily Emacs commands. in case, however, the new
command is itself a versor command, we save the item marking as a list
of conses of start . end, in versor:latest-items."
  (setq versor:latest-items
	(mapcar 
	 (lambda (item)
	   (cons (overlay-start item) (overlay-end item)))
	 versor-items))
  ;; (versor:display-item-list (format "starting command %S" this-command) versor:latest-items)
  (delete-versor-overlay))

(defun versor-set-item-indication (arg)
  "Set whether item indication is done.
Positive argument means on, negative or zero is off."
  (interactive "p")
  (if (> arg 0)
      (progn
	(message "Item indication turned on")
	(setq versor-indicate-items t)
	(add-hook 'pre-command-hook 'versor-de-indicate-current-item)
	(versor-indicate-current-item))
    (message "Item indication turned off")
    (setq versor-indicate-items nil)
    (versor-de-indicate-current-item)
    (remove-hook 'pre-command-hook 'versor-de-indicate-current-item)))

;;;; end of versor.el
