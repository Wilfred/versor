;;;; versor.el -- versatile cursor
;;; Time-stamp: <2004-01-30 14:27:17 john>
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

(defcustom versor-indicate-items nil
  "*Whether to indicate the current item after each versor movement.
The indication is cancelled at the start of the next command."
  :group 'versor
  :type 'boolean)

(defcustom versor:reversible (not (eq window-system 'x))
  "*Whether we allow reversing.
This is useful if you cannot use \"shift-next\" for \"previous\"."
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

(defvar versor-insertion-keymap (make-sparse-keymap "Versor")
  "Keymap for reading what kind of insertion to do.")

(defun versor:setup (&rest keysets)
  "Set up the versor (versatile cursor) key system.

This rebinds the cursor (arrow) keys in such a way that they can move
the cursor in other ways than just cartesian (up/down,
left/right). Instead, they take a pair of adjacent dimensions in a
series of dimensions (such as characters/lines or lines/pages).

Several such series (meta-dimensions) of dimensions are available. The
vertical arrow keys are always for the more significant of the two
dimensions, just as they are in the normal key bindings.

Ctrl-arrow for each arrow takes you to the appropriate end of the
appropriate dimension.

Meta-arrows change the dimensions. M-left and M-right shift the pair
of dimensions within the current series, and M-up and M-down change
which series you are in. The cursor colour indicates which dimension
is selected, and the current meta-dimension and dimension are shown
in the mode line.

If you set the variable versor-indicate-items (which see) the current
item is highlighted after each versor move. (The highlighting is removed
at the next command, so versor can co-operate unobtrusively with
traditional character-based editing.)

DEL deletes the current item.

The meta-dimensions and dimensions defined by default are:
  cartesian (char, line, page)
  structural (char, expression, expression depth, defun)
  natural language oriented (char, word, phrase, sentence, paragraph)
  tabular text (HTML table rows and cells)
  programming language oriented (expression, part of statement, statement)

The arguments can be any (combination) of
  'arrows        -- for the main cursor keys
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
    (global-set-key [ C-left ]  'versor:start)
    (global-set-key [ C-right ] 'versor:end)
    (global-set-key [ home ]    'versor:start)
    (global-set-key [ end ]     'versor:end)
  
    (global-set-key [ up ]      'versor:over-prev)
    (global-set-key [ down ]    'versor:over-next)
    (global-set-key [ M-up ]    'versor:prev-meta-level)
    (global-set-key [ M-down ]  'versor:next-meta-level)
    (global-set-key [ C-up ]    'versor:over-start)
    (global-set-key [ C-down ]  'versor:over-end)
    (global-set-key [ C-home ]  'versor:over-start)
    (global-set-key [ C-end ]   'versor:over-end)

    (global-set-key [ prior ]   'versor:over-over-prev)
    (global-set-key [ next ]    'versor:over-over-next)
    (global-set-key [ C-prior ] 'versor:over-over-start)
    (global-set-key [ C-next ]  'versor:over-over-end)
    )

  (when (memq 'keypad keysets)
    (global-set-key [ kp-left ]    'versor:prev)
    (global-set-key [ kp-right ]   'versor:next)
    (global-set-key [ M-kp-left ]  'versor:out)
    (global-set-key [ M-kp-right ] 'versor:in)
    (global-set-key [ C-kp-left ]  'versor:start)
    (global-set-key [ C-kp-right ] 'versor:end)
    (global-set-key [ home ]       'versor:start)
    (global-set-key [ end ]        'versor:end)
  
    (global-set-key [ kp-up ]      'versor:over-prev)
    (global-set-key [ kp-down ]    'versor:over-next)
    (global-set-key [ M-kp-up ]    'versor:prev-meta-level)
    (global-set-key [ M-kp-down ]  'versor:next-meta-level)
    (global-set-key [ C-kp-up ]    'versor:over-start)
    (global-set-key [ C-kp-down ]  'versor:over-end)
    (global-set-key [ C-home ]     'versor:over-start)
    (global-set-key [ C-end ]      'versor:over-end)

    (global-set-key [ prior ]      'versor:over-over-prev)
    (global-set-key [ next ]       'versor:over-over-next)
    (global-set-key [ C-prior ]    'versor:over-over-start)
    (global-set-key [ C-next ]     'versor:over-over-end)

    )

  (when (memq 'keypad-misc keysets)
    (global-set-key [ kp-enter ]  'versor:copy)
    (global-set-key [ kp-insert ] 'versor:insert)
    (global-set-key [ kp-delete ] 'versor:kill)
    (global-set-key [ kp-add ]    'other-window)
    (cond
     ((memq 'arrows keysets)
      (define-key versor-insertion-keymap [ left ]  'versor-insert-before)
      (define-key versor-insertion-keymap [ right ] 'versor-insert-after)
      (define-key versor-insertion-keymap [ up ]    'versor-insert-around)
      (define-key versor-insertion-keymap [ down ]  'versor-insert-within)
      )
     ((memq 'keypad keysets)
      (define-key versor-insertion-keymap [ kp-left ]  'versor-insert-before)
      (define-key versor-insertion-keymap [ kp-right ] 'versor-insert-after)
      (define-key versor-insertion-keymap [ kp-up ]    'versor-insert-around)
      (define-key versor-insertion-keymap [ kp-down ]  'versor-insert-within)
      )))

  (global-set-key [ insert ]   'versor:insert)
  (global-set-key [ delete ]   'versor:kill)
  (global-set-key [ M-delete ] 'versor:copy)

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

(mapcar 'makunbound '(versor:current-level-name moves-moves versor:meta-level versor:level))

(defvar versor:meta-level 1
  "The current versor meta-level, as an index into moves-moves")

(defvar versor:level 1
  "The current versor level, as an index into (aref moves-moves versor:meta-level)")

(defun versor:make-movemap-set (name &rest movemaps)
  "Make a set of movemaps called NAME from the remaining arguments.
The lowest-level (finest-grain) movemap should come first."
  (apply 'vector name movemaps))

(defun versor:make-movemap (name)
  "Make a movemap called NAME."
  (list name))

(defun versor:define-move (movemap move command)
  "In MOVEMAP define MOVE to do COMMAND. Analogous to define-key."
  (let ((pair (assoc move movemap)))
    (if pair
	(rplacd pair command)
      (rplacd movemap
	      (cons (cons move
			  command)
		    (cdr movemap))))))

(defun versor:define-moves (movemap move-command-specs)
  "In MOVEMAP define each of MOVE-COMMAND-SPECS.
We can't just splice MOVE-COMMAND-SPECS into the map because that would
not interact properly with any existing definitions in the map."
  ;; (message "Defining %S" move-command-specs)
  (mapcar 
   (function
    (lambda (k-c)
      ;; (message "  Defining %S to be %S" (first k-c) (second k-c))
      (versor:define-move movemap (first k-c) (second k-c))))
   move-command-specs))

(mapcar (function 
	 (lambda (name)
	   (set (intern (concat "movemap-" name))
		(versor:make-movemap name))))
	'("chars"
	  "lines"
	  "pages"
	  "exprs"
	  "depth"
	  "statement-parts"
	  "statements"
	  "defuns"
	  "words"
	  "phrases"
	  "sentences"
	  "paragraphs"
	  "cells"
	  "rows"))

(versor:define-moves movemap-chars
		      '((color "purple")
			(first versor:start-of-line)
			(previous backward-char)
			(next forward-char)
			(last end-of-line)
			(delete delete-char)))

(versor:define-moves movemap-lines
		     '((color "black")
		       (first beginning-of-buffer)
		       (previous versor:previous-line)
		       (next versor:next-line)
		       (last end-of-buffer)
		       (end-of-item end-of-line)
		       (delete kill-line)))

(versor:define-moves movemap-pages
		     '((color "white")
		       (first beginning-of-buffer)
		       (previous scroll-down)
		       (next scroll-up)
		       (last end-of-buffer)))

(versor:define-moves movemap-exprs
		     '((color "green")
		       (first first-sexp)
		       (previous previous-sexp)
		       (next next-sexp)
		       (end-of-item forward-sexp)
		       (last last-sexp)
		       (mark mark-sexp)
		       (delete kill-sexp)))

(versor:define-moves movemap-depth
		     '((color "orange")
		       (first beginning-of-defun)
		       (previous backward-up-list)
		       (next down-list)
		       (last innermost-list)))

(versor:define-moves movemap-statement-parts
		     '((color "blue")
		       (first navigate-head)
		       (previous statement-navigate-parts-previous)
		       (next statement-navigate-parts-next)
		       (last navigate-body)
		       (end-of-item latest-statement-navigation-end)))

(versor:define-moves movemap-statements
		     '((color "cyan")
		       (first beginning-of-defun)
		       (previous beginning-of-statement)
		       (next end-of-statement)
		       (last end-of-defun)
		       (end-of-item latest-statement-navigation-end)))

(versor:define-moves movemap-defuns
		     '((color "yellow")
		       (first versor:first-defun)
		       (previous versor:previous-defun)
		       (next versor:next-defun)
		       (end-of-item end-of-defun)
		       (last versor:last-defun)))

(versor:define-moves movemap-words
		     '((color "grey")
		       (first backward-phrase)
		       (previous backward-word)
		       (next next-word)
		       (end-of-item forward-word)
		       (last forward-phrase)))

(versor:define-moves movemap-phrases
		     '((color "blue")
		       (first backward-sentence)
		       (previous backward-phrase)
		       (next forward-phrase)
		       (last forward-sentence)))

(versor:define-moves movemap-sentences
		     '((color "cyan")
		       (first backward-paragraph)
		       (previous backward-sentence)
		       (next forward-sentence)
		       (last forward-paragraph)))

(versor:define-moves movemap-paragraphs
		     '((color "yellow")
		       (first beginning-of-buffer)
		       (previous backward-paragraph)
		       (next forward-paragraph)
		       (last end-of-buffer)))

(versor:define-moves movemap-blocks
		     '((color "green")
		       (previous nested-blocks-backward)
		       (next nested-blocks-forward)))

(versor:define-moves movemap-block-depth
		     '((color "orange")
		       (previous nested-blocks-leave)
		       (next nested-blocks-enter)))

(versor:define-moves movemap-cells
		     '((color "blue")
		       (first versor:first-cell)
		       (previous versor:previous-cell)
		       (next versor:next-cell)
		       (last versor:last-cell)))

(versor:define-moves movemap-rows
		     '((color "cyan")
		       (first versor:first-row)
		       (previous versor:previous-row)
		       (next versor:next-row)
		       (last versor:last-row)))
		       
(setq moves-cartesian (versor:make-movemap-set "cartesian"
					       movemap-chars
					       movemap-lines
					       movemap-pages)

      moves-structural (versor:make-movemap-set "structural"
						movemap-chars
						movemap-exprs
						movemap-depth
						movemap-defuns)

      moves-text (versor:make-movemap-set "text"
					  movemap-chars
					  movemap-words
					  movemap-phrases
					  movemap-sentences
					  movemap-paragraphs)

      moves-structured-text (versor:make-movemap-set "structured text"
						     movemap-chars
						     movemap-words
						     movemap-blocks
						     movemap-block-depth)

      moves-tables (versor:make-movemap-set "tables"
					    movemap-chars
					    movemap-cells
					    movemap-rows)

      moves-program (versor:make-movemap-set "program"
					     movemap-chars
					     movemap-exprs
					     movemap-statement-parts
					     movemap-statements
					     movemap-defuns))

(defvar moves-moves
  (versor:make-movemap-set "metamoves"
			   moves-cartesian
			   moves-structural
			   moves-text
			   moves-structured-text
			   moves-tables
			   moves-program)
  "The map of meta-moves.")

(defmacro versor:current-meta-level ()
  "The current meta-level, as an array."
  '(aref moves-moves versor:meta-level))

(defun versor:current-level (&optional level-offset)
  "The current level, as an array.
With optional LEVEL-OFFSET, add that to the level first."
  (if (integerp level-offset)
      (let ((meta (versor:current-meta-level)))
	(aref meta (min (+ versor:level level-offset)
			(1- (length meta)))))
    (aref (versor:current-meta-level) versor:level)))

(defun versor:action (level action)
  "From LEVEL get ACTION."
  (cdr (assoc action level)))

(defvar versor:current-level-name (first (versor:current-level))
  "The name of the current versor level, for display in the global-mode-string")

(defvar versor:current-meta-level-name (aref (versor:current-meta-level) 0)
  "The name of the current versor meta-level, for display in the global-mode-string")

(defun versor::trim-level ()
  "Ensure that versor:level is in range."
  (let ((max (1- (length (versor:current-meta-level)))))
    (when (> versor:level max)
      (setq versor:level
	    (if versor:level-wrap 1 max)))
    (when (< versor:level 1)
      (setq versor:level
	    (if versor:level-wrap max 1)))))

(defun versor::trim-meta-level ()
  "Ensure that versor:meta-level is in range."
  ;; (message "meta-level=%d, adjusting" versor:meta-level)
  (let ((max (1- (length moves-moves))))
    (when (> versor:meta-level max)
      (setq versor:meta-level
	    (if versor:meta-level-wrap 1 max)))
    (when (< versor:meta-level 1)
      (setq versor:meta-level
	    (if versor:meta-level-wrap max 1))))
  ;; (message "adjusted meta-level=%d" versor:meta-level)
  )

(defun versor:set-status-display ()
  "Indicate the state of the versor system."
  (message (first (versor:current-level)))
  (setq versor:current-level-name (first (versor:current-level))
	versor:current-meta-level-name (aref (versor:current-meta-level) 0))
  (if versor:reversible
      (setq versor:mode-line-begin-string (if versor:reversed " <==" " <")
	    versor:mode-line-end-string (if versor:reversed ">" "==>"))
    (setq versor:mode-line-begin-string " <"
	  versor:mode-line-end-string ">"))
  (force-mode-line-update t)
  (when t (set-cursor-color (versor:action (versor:current-level) 'color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; underlining current item ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar versor-items (list (cons (cons nil nil) nil))
  "The current versor items.
There is a separate value of this variable for each buffer.
There is always at least one item in this list, to avoid churning
overlays; if there is no current item, the top item has nil for its
start and end.
Each item is ((start . end) . overlay)")
(make-variable-buffer-local 'versor-items)

(defun versor:set-current-item (start end)
  "Set the start and end of the current item."
  (let ((item (car versor-items)))
    (rplaca (car item) start)
    (rplacd (car item) end)))

(defun versor:get-current-item ()
  "Return (start . end) for the current item."
  (if (versor:current-item-valid)
      (car (car versor-items))
    (let* ((end (progn (funcall (or (versor:get-action 'end-of-item)
				    (versor:get-action 'next))
				1)
		       (point)))
	   (start (progn (funcall (versor:get-action 'previous) 1)
			 (point))))
      (cons start end))))

(defun versor:current-item-valid ()
  "Return whether there is a valid current item."
  (let ((item-pair (car (car versor-items))))
    (and (car item-pair) (cdr item-pair))))

(defun versor-item-overlay ()
  "The item indication overlay for this buffer."
  (cdr (car versor-items)))

(defun make-versor-overlay (start end)
  "Make a versor overlay at START END.
If there is already a versor overlay for this buffer, reuse that."
  (unless (overlayp (versor-item-overlay))
    (let ((overlay (make-overlay start end (current-buffer))))
      (rplacd (car versor-items) overlay)
      (overlay-put overlay 'face 'region)))
  (move-overlay (versor-item-overlay) start end (current-buffer)))

(defun delete-versor-overlay ()
  "Delete the versor overlay for this buffer."
  ;; in fact, we just disconnect it from the buffer
  (let ((overlay (versor-item-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay))))

(defun versor-clear-current-item-indication ()
  "Intended to go at the start of each versor motion command.
It clears variables looked at by versor-indicate-current-item, which sets them
up for itself only if the command didn't set them for it."
  (versor:set-current-item nil nil)
  (setq versor-indication-known-valid nil)
  (delete-versor-overlay))

(defun versor-indicate-current-item ()
  "Mark the current item.
Intended to be called at the end of all versor commands."

  ;; It would be nice to make this try to get all of the item visible in the window

  ;; It makes me aware that many of the movements need redefining,
  ;; to be consistent over quite where they end up!

  (unless (versor:current-item-valid)
    (versor:set-current-item
     ;; the command we have just run may have set these for us, if it knows in such a way that
     ;; it can tell us much faster than we could work out for ourselves;
     ;; they are cleared at the start of each command by versor-clear-current-item-indication,
     ;; which, like us, is called by the as-versor-command macro
     (point)
     (versor-end-of-item)))

  (when versor-indicate-items
    ;; re-do this because it somehow gets taken off from time to time
    (add-hook 'pre-command-hook 'versor-de-indicate-current-item)
    (versor-de-indicate-current-item) ; make sure there is only one --- shouldn't need this here???????????????
    (let ((pair (versor:get-current-item)))
      (make-versor-overlay (car pair) (cdr pair)))))

(defun versor-de-indicate-current-item ()
  "Remove the current item marking.
Intended to go on pre-command-hook."
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

;;;;;;;;;;;;;;;;;;;
;;;; reversing ;;;;
;;;;;;;;;;;;;;;;;;;

(defvar versor:reversed nil
  "Whether we have currently reversed the navigation directions.
This is useful if you cannot use \"shift-next\" for \"previous\".
It is enabled by the variable versor:reversible, which see.")

(defvar versor:mode-line-begin-string " <"
  "String corresponding to bracket for beginning of versor markers according to versor:reversed.")
(defvar versor:mode-line-end-string (if versor:reversible "==>" ">")
  "String corresponding to bracket for end of versor markers according to versor:reversed.")

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; more structure ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro as-versor-motion-command (&rest body)
  "Run BODY as a versor motion command.
Various pre- and post-processing get done."
  `(progn
     (versor-clear-current-item-indication)
     (progn
       ,@body)
     (versor-indicate-current-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; commands begin here ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; meta-dimension commands

(defun versor:out ()
  "Move versor:level out a dimension in (versor:current-meta-level)."
  (interactive)
  (as-versor-motion-command
   (setq versor:old-level versor:level)
   (if (and (interactive-p)
	    versor:reversed)
       (decf versor:level)
     (incf versor:level))
   (versor::trim-level)
   (versor:set-status-display)))

(defun versor:in ()
  "Move versor:level in a dimension in (versor:current-meta-level)."
  (interactive)
  (as-versor-motion-command
   (setq versor:old-level versor:level)
   (if (and (interactive-p)
	    versor:reversed)
       (incf versor:level)
     (decf versor:level))
   (versor::trim-level)
   (versor:set-status-display)))

(defun versor:next-meta-level ()
  "Move to the next meta-level."
  (interactive)
  (as-versor-motion-command
   (if (and (interactive-p)
	    versor:reversed)
       (decf versor:meta-level)
     (incf versor:meta-level))
   (versor::trim-meta-level)
   (versor::trim-level)
   (versor:set-status-display)))

(defun versor:prev-meta-level ()
  "Move to the previous meta-level."
  (interactive)
  (as-versor-motion-command
   (if (and (interactive-p)
	    versor:reversed)
       (incf versor:meta-level)
     (decf versor:meta-level))
   (versor::trim-meta-level)
   (versor::trim-level)
   (versor:set-status-display)))

;;;; commands within the current dimension(s)

(defun versor:reverse ()
  "Reverse the forward and backward directions."
  (interactive)
  (setq versor:reversed (not versor:reversed))
  (versor:set-status-display))

(defun versor:get-action (action &optional level-offset)
  "Get ACTION for the current versor level, taking modal bindings into account.
With optional LEVEL-OFFSET, add that to the level first.
Modal bindings are made by versor:bind-modal, which is in versor-modal.el"
  (let* ((level-data (versor:current-level level-offset))
	 (level-modal-data (cdr (assoc major-mode (versor:action level-data 'modal-bindings))))
	 (modal-binding (if level-modal-data (versor:action level-modal-data action) nil)))
    (if modal-binding
	modal-binding
      (versor:action level-data action))))

(defvar versor:point-before-move nil
  "The value of point before the latest versor move.")

(defun versor:start (&optional level-offset)
  "Move backward to the start of the current dimension.
With optional LEVEL-OFFSET, add that to the level first.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (as-versor-motion-command
   (unless (run-hook-with-args-until-success
	    (if versor:reversed
		'versor:end-hooks
	      'versor:start-hooks))
     (if (eq last-command this-command)
	 (progn
	   (versor:prev-meta-level)
	   (goto-char versor:point-before-move))
       (progn
	 (setq versor:point-before-move (point))
	 (call-interactively
	  (versor:get-action (if versor:reversed 'last 'first) level-offset)))))))

(defun versor:prev-action (&optional level-offset)
  "Internal action code to move backward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (unless (run-hook-with-args-until-success 'versor:prev-hooks)
    (setq versor:point-before-move (point))
    (call-interactively (versor:get-action 'previous level-offset))))

(defun versor:next-action (&optional level-offset)
  "Internal action code to move forward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (unless (run-hook-with-args-until-success 'versor:next-hooks)
    (setq versor:point-before-move (point))
    (call-interactively (versor:get-action 'next level-offset))))

(defun versor:prev (&optional level-offset)
  "Move backward within the current dimension."
  (interactive '(nil))
  (as-versor-motion-command
   (if versor:reversed
       (versor:next-action level-offset)
     (versor:prev-action level-offset))))

(defun versor:next (&optional level-offset)
  "Move forward within the current dimension."
  (interactive '(nil))
  (as-versor-motion-command
   (if versor:reversed
       (versor:prev-action level-offset)
     (versor:next-action level-offset))))

(defun versor:end (&optional level-offset)
  "Move forward to the end of the current dimension.
With optional LEVEL-OFFSET, add that to the level first.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (as-versor-motion-command
   (unless (run-hook-with-args-until-success
	    (if versor:reversed
		'versor:start-hooks
	      'versor:end-hooks))
     (if (eq last-command this-command)
	 (progn
	   (versor:next-meta-level)
	   (goto-char versor:point-before-move))
       (progn
	 (setq versor:point-before-move (point))
	 (call-interactively
	  (versor:get-action (if versor:reversed 'first 'last) level-offset)))))))

(defun versor:over-start ()
  "Move backward to the start of the dimension above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:start 1))

(defun versor:over-prev ()
  "Move backward within the dimension above the current one."
  (interactive)
  (versor:prev 1))

(defun versor:over-next ()
  "Move forward within the dimension above the current one."
  (interactive)
  (versor:next 1))

(defun versor:over-end ()
  "Move forward to the end of the dimension above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:end 1))

(defun versor:over-over-start ()
  "Move backward to the start of the dimension two above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:start 2))

(defun versor:over-over-prev ()
  "Move backward within the dimension two above the current one."
  (interactive)
  (versor:prev 2))

(defun versor:over-over-next ()
  "Move forward within the dimension two above the current one."
  (interactive)
  (versor:next 2))

(defun versor:over-over-end ()
  "Move forward to the end of the dimension two above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:end 2))

(defun versor-end-of-item ()
  "Return the end of the current item."
  (let ((mover (or (versor:get-action 'end-of-item)
		   (versor:get-action 'next))))
    (if mover
	(save-excursion
	  (funcall mover 1)
	  (point))
      nil)))

(defun versor:copy ()
  "Copy a unit of the current dimension."
  (interactive)
  (let* ((item (versor:get-current-item)))
    (copy-region-as-kill (car item) (cdr item)))
  (versor-indicate-current-item))

(defun versor:mark ()
  "Mark a unit of the current dimension."
  (interactive)
  (let ((ready-made (versor:get-action 'mark)))
    (if ready-made
	(call-interactively ready-made)
      (let* ((item (versor:get-current-item)))
	(let ((transient-mark-mode t))
	  (set-mark (cdr item))
	  (goto-char (car item))
	  (sit-for 1))))))

(defun versor:kill ()
  "Kill a unit of the current dimension."
  (interactive)
  (let ((ready-made (versor:get-action 'delete)))
    (if ready-made
	(call-interactively ready-made)
      (let* ((item (versor:get-current-item)))
	(kill-region (car item) (cdr item)))))
  (versor-indicate-current-item))

(defun versor:insert ()
  "Insert something relative to the current versor item."
  (interactive)
)

;;;; some internal functions for operations that aren't typically there already

(defun versor:start-of-line (n)
  "Move to first non-space on line, or to start of Nth line from here."
  (interactive "p")
  (if (or (= n 1)
	  (not (eq last-command 'versor:start-of-line)))
      (back-to-indentation)
    (beginning-of-line n)))

(defmacro versor:following-margins (&rest body)
  "Perform BODY while sticking to the same margin if on one."
  `(let* ((at-left-edge (bolp))
	  (at-right-margin (eolp))
	  (at-left-margin
	   (or				; don't calculate this if either of the others hold
	    at-right-margin		; as it is slower, and is not used if the others are
	    at-left-edge
	    (save-excursion (skip-syntax-backward "\\s-") (bolp)))))
     ,@body
     (cond
      (at-left-edge (beginning-of-line 1))
      (at-right-margin (end-of-line 1))
      (at-left-margin (back-to-indentation)))))

(defun versor:previous-line (n)
  "Move to the previous line, following margins if on one."
  (interactive "p")
  (versor:following-margins (previous-line n)))

(defun versor:next-line (n)
  "Move to the next line, following margins if on one."
  (interactive "p")
  (versor:following-margins (next-line n)))

;; (defun other-window-backwards (n)
;;   "Select the -Nth window -- see other-window, this just negates the argument."
;;   (interactive "p")
;;   (other-window (- n)))

;; (defun first-window ()
;;   "Select the first window in the frame"
;;   (interactive)
;;   (select-window (window-at 0 0)))

;; (defun last-window ()
;;   "Select the last window in the frame"
;;   (interactive)
;;   (select-window (window-at (1- (frame-width)) (- (frame-height) 3))))

;; (defun zero-char ()
;;   "Make the character at point be zero."
;;   (save-excursion
;;     (delete-region (point) (1+ (point)))
;;     (insert 0)))

;; (defun dec-char ()
;;   "Decrement the character at point."
;;   (interactive)
;;   (save-excursion
;;     (let ((new-char (1- (char-after (point)))))
;;       (delete-region (point) (1+ (point)))
;;       (insert new-char))))

;; (defun inc-char ()
;;   "Increment the character at point."
;;   (interactive)
;;   (save-excursion
;;     (let ((new-char (1+ (char-after (point)))))
;;       (delete-region (point) (1+ (point)))
;;       (insert new-char))))

;; (defun max-char ()
;;   "Make the character at point be zero."
;;   (save-excursion
;;     (delete-region (point) (1+ (point)))
;;     (insert -1)))

(defun safe-scan-sexps (from count)
  "Like scan-sexps, but returns nil on error."
  (condition-case error-var
      (scan-sexps from count)
    (error nil)))

(defun safe-scan-lists (from count depth)
  "Like scan-lists, but returns nil on error."
  (condition-case
      error-var
      (scan-lists from count depth)
    (error nil)))

(defun first-sexp ()
  "Move back by sexps until you can go back no more."
  (interactive)
  (let ((backto (safe-scan-sexps (point) -1)))
    (while backto
      (goto-char backto)
      (setq backto (safe-scan-sexps backto -1)))))

(defun last-sexp ()
  "Move forward by sexps until you can go no more."
  (interactive)
  (let ((onto (safe-scan-sexps (point) 1))
	(prev (point))
	(prevprev (point)))
    (while onto
      (goto-char onto)
      (setq prevprev prev
	    prev onto
	    onto (safe-scan-sexps onto 1)))
    (parse-partial-sexp prevprev prev
			0 t)))

(defun next-sexp (n)
  "Move forward N sexps.
Like forward-sexp but moves to the start of the next sexp rather than the
end of the current one, and so skips leading whitespace etc."
  (interactive "p")
  (let* ((where (safe-scan-sexps (point) n))
	 (one-more (safe-scan-sexps where 1)))
    (if (and where one-more)
	(progn
	  (goto-char where)
	  (let ((limit (save-excursion (last-sexp) (point))))
	    (when (> n 0)
	      (parse-partial-sexp where limit
				  0 t))))
      (message "No more sexps"))))

(defun previous-sexp (n)
  "Move backward N sexps.
Like backward-sexp but stops without error on reaching the start."
  (interactive "p")
  (let ((where (safe-scan-sexps (point) (- n))))
    (if where
      (goto-char where)
      (message "No more previous sexps"))))

(defun innermost-list ()
  "Move in by sexps until you can go in no more."
  (interactive)
  (let ((p (point))
	(n nil))
    (while (setq n (safe-scan-lists p 1 -1))
      (setq p n))
    (goto-char p)))

(defun next-word (n)
  "Move forward a word, or, with argument, that number of words.
Like forward-word but leaves point on the first character of the word,
and never on the space or punctuation before it."
  (interactive "p")
  (forward-word n)
  (skip-syntax-forward "^w"))

(defun forward-phrase (n)
  "Move forward a phrase, or, with argument, that number of phrases.
Stay within the current sentence."
  (interactive "p")
  (let ((sentence (save-excursion
		    (forward-sentence 1)
		    (point))))
    (while (> n 0)
      (unless (re-search-forward phrase-end sentence 'stay)
	(setq n 0))
      (decf n))))

(defun backward-phrase (n)
  "Move backward a phrase.
Stay within the current sentence."
  (interactive "p")
  (let ((sentence (save-excursion
		    (backward-sentence 1)
		    (point))))
    (while (> n 0)
      (unless (re-search-backward phrase-end sentence 'stay)
	(setq n 0))
      (decf n))))

(defun next-buffer ()
  "Select the next buffer in this window."
  (interactive)
  (let ((this-buffer (current-buffer)))
    (switch-to-buffer (other-buffer this-buffer))
    (bury-buffer this-buffer)))

(defun previous-buffer ()
  "Select the previous buffer in this window."
  (interactive)
  (switch-to-buffer (car (last (buffer-list)))))

(defun last-buffer ()
  "Select the last buffer in this window."
  (interactive)
)

(defun first-buffer ()
  "Select the first buffer in this window."
  (interactive)
)

(defun tempo-first-mark ()
  "Go to the first tempo marker."
  (interactive)
  (let ((first-mark (first tempo-marks)))
    (when first-mark (goto-char first-mark))))

(defun tempo-last-mark ()
  "Go to the last tempo marker."
  (interactive)
  (let ((last-mark (last tempo-marks)))
    (when last-mark (goto-char last-mark))))

(defun versor:first-cell ()
  "Move to the first cell."
  (interactive)
  (if (and (search-backward "<tr" (point-min) t)
	   (re-search-forward "<t[dh][^>]+" (point-max) t))
      t
    (error "Could not locate first cell.")))

(defun versor:previous-cell (n)
  "Move to the previous cell."
  (interactive "p")
  (if (re-search-backward "</t[dh][^>]+" (point-min) t)
      (progn
	(while (> n 0)
	  (if (not (re-search-backward "<t[dh][^>]+" (point-min) t))
	      (error "No more previous cells")
	    (decf n)))
	(goto-char (match-end 0)))
    (error "Could not end of previous locate cell")))

(defun versor:next-cell (n)
  "Move to the next cell."
  (interactive "p")
  (while (> n 0)
    (if (not (re-search-forward "<t[dh][^>]+" (point-max) t))
	(error "No more next cells")
      (decf n))))

(defun versor:last-cell ()
  "Move to the last cell."
  (interactive)
  (if (and (search-forward "</tr" (point-max) t)
	   (re-search-backward "<t[dh][^>]+" (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last cell")))

(defun versor:first-row ()
  "Move to the first row."
  (interactive)
  (if (and (search-backward "<table" (point-min) t)
	   (re-search-forward "<tr[^>]+" (point-max) t))
      t
    (error "Could not locate first row")))

(defun versor:previous-row (n)
  "Move to the previous row."
  (interactive "p")
  (if (re-search-backward "</tr[^>]+" (point-min) t)
      (progn
	(while (> n 0)
	  (if (not (re-search-backward "<tr[^>]+" (point-min) t))
	      (error "No more previous rows")
	    (decf n)))
	(goto-char (match-end 0)))
    (error "Could not end of previous locate row")))

(defun versor:next-row (n)
  "Move to the next row."
  (interactive "p")
  (while (> n 0)
    (if (not (re-search-forward "<tr[^>]+" (point-max) t))
	(error "No more next rows")
      (decf n))))

(defun versor:last-row ()
  "Move to the last row."
  (interactive)
  (if (and (search-forward "</table" (point-max) t)
	   (re-search-backward "<tr[^>]+" (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last row")))

(defun versor:first-defun ()
  "Move to the start of the first function definition."
  (interactive)
  (goto-char (point-min))
  (beginning-of-defun -1))

(defun versor:previous-defun (n)
  "Move to the start of the previous function definition."
  (interactive "p")
  (beginning-of-defun n))

(defun versor:next-defun (n)
  "Move to the start of the next function definition."
  (interactive "p")
  (beginning-of-defun (- n)))

(defun versor:last-defun ()
  "Move to the start of the last function definition."
  (interactive)
  (goto-char (point-max))
  (beginning-of-defun 1))

;;;; end of versor.el
