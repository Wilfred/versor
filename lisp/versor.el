;;; versor.el -- versatile cursor
;;; Time-stamp: <2004-05-17 14:00:51 john>
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

(mapcar 'makunbound '(versor:current-level-name moves-moves versor:meta-level versor:level))

(defvar versor:meta-level 1
  "The current versor meta-level, as an index into moves-moves")

(defvar versor:level 1
  "The current versor level, as an index into (aref moves-moves versor:meta-level)")

(defun versor:make-movemap-set (name &rest movemaps)
  "Make a set of movemaps called NAME from the remaining arguments.
The lowest-level (finest-grain) movemap should come first.
A movemap-set represents a metalevel of movements.
Also, all the movemap-sets are grouped together using another movemap-set,
called moves-moves, which is the root variable of the versor system."
  (apply 'vector name movemaps))

(defun versor:make-movemap (name)
  "Make a movemap called NAME.
A move map is a list whose head is the name of the map,
and whose tale is an alist of moves and the commands which execute them.
Moves are named by the following symbols, and possibly others added since
this documentation was written:
  previous
  next
  first
  last
  mark
  delete
  end-of-item
  color
The pseudo-move \"color\" gives the cursor colour to use when this move map
is current.
you can fill in the contents of a move map by using versor:define-move and
versor:define-moves.
Move maps are grouped together by versor:make-movemap-set."
  (list name))

(defun versor:define-move (movemap move command)
  "In MOVEMAP define MOVE to do COMMAND. Analogous to define-key.
See the definition of versor:make-movemap for details of move maps."
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
not interact properly with any existing definitions in the map.
See the definition of versor:make-movemap for details of move maps."
  (mapcar 
   (function
    (lambda (k-c)
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
	  "blocks"
	  "block-depth"
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
		       (:underline "dark green")
		       (:background "pale green")
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
		       (previous versor-backward-up-list)
		       (next versor-down-list)
		       (last innermost-list)))

(versor:define-moves movemap-statement-parts
		     '((color "blue")
		       (first navigate-this-head)
		       (previous statement-navigate-parts-previous)
		       (next statement-navigate-parts-next)
		       (last navigate-this-body)
		       (end-of-item latest-statement-navigation-end)))

(versor:define-moves movemap-statements
		     '((color "cyan")
		       (first beginning-of-defun)
		       (previous previous-statement)
		       (next next-statement)
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
		       (:background "light gray")
		       (:underline "dark slate gray")
		       ;; things like this (notionally the wrong
		       ;; dimension) still work OK, because of how
		       ;; versor-indicate-current-item works when the
		       ;; things it calls don't explicitly set the item
		       ;; boundaries for it:
		       (first backward-phrase) 
		       (previous backward-word)
		       (next next-word)
		       (end-of-item forward-word)
		       (last forward-phrase)))

(versor:define-moves movemap-phrases
		     '((color "blue")
		       (:background "cornflower blue")
		       (first backward-sentence)
		       (previous backward-phrase)
		       (next forward-phrase)
		       (last forward-sentence)))

(versor:define-moves movemap-sentences
		     '((color "cyan")
		       (:background "light sky blue")
		       (first versor:backward-paragraph)
		       (previous backward-sentence)
		       (next forward-sentence)
		       (last versor:forward-paragraph)))

(versor:define-moves movemap-paragraphs
		     '((color "yellow")
		       (first beginning-of-buffer)
		       (previous versor:backward-paragraph)
		       (next versor:forward-paragraph)
		       (end-of-item forward-paragraph)
		       (last end-of-buffer)))

(versor:define-moves movemap-blocks
		     '((color "green")
		       (:underline "dark green")
		       (:background "pale green")
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


;; See versor:make-movemap-set for details of movemap-sets
		       
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
  "The map of meta-moves.
See versor:make-movemap-set for the description of move map sets.
Note that this is a reuse of that data type at a different level. ")

(defmacro versor:current-meta-level ()
  "The current meta-level, as an array."
  '(aref moves-moves versor:meta-level))

(defun versor:current-level (&optional level-offset)
  "Return the current level, as an array.
With optional LEVEL-OFFSET, add that to the level first."
  (if (integerp level-offset)
      (let ((meta (versor:current-meta-level)))
	(aref meta (min (+ versor:level level-offset)
			(1- (length meta)))))
    (aref (versor:current-meta-level) versor:level)))

(defun versor:action (level action)
  "From LEVEL get ACTION, which will be a move such as next or previous."
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
  (let ((max (1- (length moves-moves))))
    (when (> versor:meta-level max)
      (setq versor:meta-level
	    (if versor:meta-level-wrap 1 max)))
    (when (< versor:meta-level 1)
      (setq versor:meta-level
	    (if versor:meta-level-wrap max 1)))))

(defvar versor:mode-current-levels nil
  "Alist of mode name symbols to the current meta-level and level for that mode.
Used by versor-local, but set here."
  ;; I tried getting versor-local's versor:mode-change-function to remember the
  ;; levels for the mode, but couldn't get it to work -- something about the
  ;; mode being set strangely in the minibuffer, I think
)

(defvar versor:item-face (make-face 'versor-item)
  "Face to use for versor items")

(defvar versor:use-face-attributes
  (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 21))
  "Whether to use face attributes, as provided from Emacs 21 onwards.")

(when versor:use-face-attributes
  (set-face-attribute 'versor-item nil :inherit 'region))

(defun versor:set-status-display (&optional one of-these)
  "Indicate the state of the versor system."
  (setq versor:current-level-name (first (versor:current-level))
	versor:current-meta-level-name (aref (versor:current-meta-level) 0))
  (if one
      (if of-these
	  (versor:display-highlighted-choice one of-these)
	(if (stringp one)
	    (message one)))
    (message (first (versor:current-level))))
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
      (rplacd (cdr old-pair) versor:level)))
  )

(defun versor:display-highlighted-choice (one of-these-choices)
  "Display, with ONE highlighted, the members of OF-THESE-CHOICES"
  (let* ((msg (mapconcat
	       (lambda (string)
		 (if (string= string one)
		     (if versor:use-face-attributes
			 (let ((strong (copy-sequence string)))
			   (put-text-property 0 (length string)
					      'face 'versor-item
					      strong)
			   strong)
		       (format "[%s]" string))
		   string))
	       of-these-choices
	       ", ")))
    (message msg)))

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
Necessary pre- and post-processing get done."
  `(progn
     (versor-clear-current-item-indication)
     (progn
       ,@body)
     ;; the few commands that want to do otherwise, must re-set this
     ;; one just after using this macro
     (setq versor-extension-direction nil)
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
   (versor:set-status-display
    (first (versor:current-level))
    (mapcar 'first (versor:level-names)))))

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
   (versor:set-status-display
    (first (versor:current-level))
    (mapcar 'first (versor:level-names)))))

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
   (versor:set-status-display
     (aref (versor:current-meta-level) 0)
     (mapcar 'first (versor:meta-level-names)))))

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
   (versor:set-status-display
     (aref (versor:current-meta-level) 0)
     (mapcar 'first (versor:meta-level-names)))))

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
  "Move backward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive '(nil))
  (as-versor-motion-command
   (if versor:reversed
       (versor:next-action level-offset)
     (versor:prev-action level-offset))))

(defun versor:next (&optional level-offset)
  "Move forward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
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

(defun versor-end-of-item-position ()
  "Return the end of the current item."
  (let ((result
	 (let ((mover (or (versor:get-action 'end-of-item)
			  (versor:get-action 'next))))
	   (if mover
	       (save-excursion
		 (funcall mover 1)
		 (point))
	     nil))))
    result))

(defun versor-end-of-item ()
  "Move to the end of the current item."
  ;; perhaps this should change to move among the parts of a multipart item?
  (interactive)
  (let* ((items versor:latest-items)
	 (item (car items)))
    (as-versor-motion-command
     (goto-char
      (if (versor:current-item-valid)
	  (cdr (versor:get-current-item))
	(versor-end-of-item-position)))
     (make-versor-overlay (car item) (cdr item)))))

(defun versor-start-of-item ()
  "Move to the start of the current item."
  (interactive)
  (as-versor-motion-command
   (if (versor:current-item-valid)
       (goto-char (car (versor:get-current-item)))
     (versor:prev-action 1))))

;;;;;;;;;;;;;;;;;;;;;
;;;; extend item ;;;;
;;;;;;;;;;;;;;;;;;;;;

(defvar versor-extension-direction nil
  "Which direction versor is extending in.
Once you are extending forwards, versor-extend-item-backwards will
reduce the extension rather than extending backwards, and vice versa.")

(make-variable-buffer-local 'versor-extension-direction)

(defun versor-extend-item-forwards (&optional level-offset)
  "Add another of the current unit to the end of the item.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive)
  ;; must go outside as-versor-motion-command as that sets
  ;; versor-extension-direction, and we are about to preserve that
  ;; variable to protect it from as-versor-motion-command
  (unless versor-extension-direction
    (setq versor-extension-direction 'forwards))
  (let ((direction versor-extension-direction))
    (as-versor-motion-command
     (let* ((item (versor:get-current-item))
	    (start (versor-overlay-start item))
	    (end (versor-overlay-end item)))
       (versor:next level-offset)
       (if (eq direction 'forwards)
	   (versor:set-current-item start (versor-end-of-item-position))
	 (versor:set-current-item (point) end))))
    (setq versor-extension-direction direction)))

(defun versor-extend-item-backwards (&optional level-offset)
  "Add another of the current unit to the start of the item.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive)
  ;; must go outside as-versor-motion-command as that sets
  ;; versor-extension-direction, and we are about to preserve that
  ;; variable to protect it from as-versor-motion-command
  (unless versor-extension-direction
    (setq versor-extension-direction 'backwards))
  (let ((direction versor-extension-direction))
    (as-versor-motion-command
     (let* ((item (versor:get-current-item))
	    (start (versor-overlay-start item))
	    (end (versor-overlay-end item)))
       (versor:prev level-offset)
       (if (eq direction 'backwards)
	   (versor:set-current-item (point) end)
	 (versor:set-current-item start (versor-end-of-item-position)))))
    (setq versor-extension-direction direction)))

(defun versor-extend-over-item-forwards ()
  "Add another of the unit above the current one, to the end of the item."
  (interactive)
  (versor-extend-item-forwards 1))

(defun versor-extend-over-item-backwards ()
  "Add another of the unit above the current one, to the start of the item."
  (interactive)
  (versor-extend-item-backwards 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kill-ring operations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun versor:copy ()
  "Copy the current item."
  (interactive)
  (as-versor-motion-command
   ;; (versor:display-item-list "versor:copy" (versor:get-current-items))
   (mapcar
    (lambda (item)
      (kill-new (buffer-substring
		 (versor-overlay-start item)
		 (versor-overlay-end item))))
    (reverse (versor:get-current-items)))))

(defun versor:mark ()
  "Mark the current item.
If there are multiple parts to the current item (for example, opening
and closing brackets, when in the depth dimension), they go into
successive elements of the kill ring, for maximum compatibility with
normal Emacs commands. Our corresponding insertion commands understand
this."
  (interactive)
  (as-versor-motion-command
   (let ((ready-made (versor:get-action 'mark)))
     (if ready-made
	 (call-interactively ready-made)
       (let* ((item (versor:get-current-item)))
	 (let ((transient-mark-mode t))
	   (set-mark (cdr item))
	   (goto-char (car item))
	   (sit-for 1)))))))

(defun versor:kill ()
  "Kill the current item.
If there are multiple parts to the current item (for example, opening
and closing brackets, when in the depth dimension), they go into
successive elements of the kill ring, for maximum compatibility with
normal Emacs commands. Our corresponding insertion commands understand
this."
  (interactive)
  (as-versor-motion-command
   (let ((ready-made (versor:get-action 'delete)))
     (if ready-made
	 (call-interactively ready-made)
       (mapcar
	(lambda (item)
	  (let ((start (versor-overlay-start item))
		(end (versor-overlay-end item)))
	    (kill-new (buffer-substring start end))
	    (delete-region start end)))
	(reverse (versor:get-current-items)))))))

(defun versor:definesert (key fun)
  "Bind KEY to FUN in the map for choosing kinds of insertion.
This lets us have uniform insertion commands with various origins of
things to insert. We want to this because of being able to insert
multipart things, using the insert around command, which we want to
work both with brackets and with things previously killed -- for
example, the result of doing versor:kill in the depth dimension, which
can delete an opening and closing bracket together."
  (if (or (stringp key) (vectorp key)) (setq key (aref key 0)))
  (let ((binding (assoc key versor:insertion-kind-alist)))
    (if binding
	(rplacd binding fun)
      (setq versor:insertion-kind-alist
	    (cons
	     (cons key fun)
	     versor:insertion-kind-alist)))))

(defun versor:top-n-kills (n)
  "Return the top N entries in the kill ring."
  (let ((result nil))
    (while (> n 0)
      (decf n)
      (push (current-kill n t) result))
    result))

(versor:definesert "\d" 'versor:top-n-kills)
(versor:definesert [ kp-delete ] 'versor:top-n-kills)
(versor:definesert [ del ] 'versor:top-n-kills)
(versor:definesert "\C-y" 'versor:top-n-kills)
(versor:definesert "(" (lambda (n) (list "(" ")")))
(versor:definesert "[" (lambda (n) (list "[" "]")))
(versor:definesert "{" (lambda (n) (list "{" "}")))
(versor:definesert "<" (lambda (n) (list "<" ">")))

(defun versor:get-insertable (n &optional prompt)
  "Return N things to insert, having asked the user for what kind of insertion this is.
This lets us do commands such as insert-around using a common framework."
  ;; should also be available for putting things into the search string --
  ;; in which case it ought to have a different name
  (let* ((key (read-char (if prompt prompt "Kind of insertion: ")))
	 (command (assoc key versor:insertion-kind-alist)))
    (if (consp command)
	(funcall (cdr command) n)
      (error "Not a valid kind of insertion"))))

(defun versor:insert-before ()
  "Insert something before the current versor item."
  (interactive)
  (as-versor-motion-command
   (let* ((new-thing (versor:get-insertable 1))
	  (current-item (versor:get-current-item)))
     (goto-char (car current-item))
     (insert new-thing))))

(defun versor:insert-after ()
  "Insert something after the current versor item."
  (interactive)
  (as-versor-motion-command
   (let* ((new-thing (versor:get-insertable 1))
	  (current-item (versor:get-current-item)))
     (goto-char (cdr current-item))
     (insert new-thing))))

(defun versor:insert-around ()
  "Insert something around to the current versor item."
  (interactive)
  (as-versor-motion-command
   (let ((current-item (versor:get-current-item))
	 (new-thing (versor:get-insertable 2 "Type of insertion around item: ")))
     (message "insert-around: current-item=%S" current-item)
     (goto-char (versor-overlay-end current-item))
     (insert (second new-thing))
     (goto-char (versor-overlay-start current-item))
     (insert (first new-thing)))))

(defun versor:insert-within ()
  "Insert something within the current versor item."
  (interactive)
  (as-versor-motion-command
   (let* ((new-thing (versor:get-insertable 1))
	  (current-item (versor:get-current-item)))
     ;; not yet sure what this really means
     )))

;; some internal functions for operations that aren't typically there already,
;; or that normal emacs does slightly differently from what we want

(defun versor:forward-paragraph (&optional n)
  "Like forward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (forward-paragraph n)
  (message "forward-paragraph %d" n)
  (if (> n 0)
      (while (looking-at "^$")
	(forward-line 1))))

(defun versor:backward-paragraph (&optional n)
  "Like backward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (backward-paragraph (1+ n))
  (message "backward-paragraph %d" n)
  (if (> n 0)
      (while (looking-at "^$")
	(forward-line 1))))

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

(defun versor-backward-up-list (&rest args)
  (interactive "p")
  (apply 'backward-up-list args)
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (make-versor-overlay (point) (1+ (point))) ; should probably be versor:set-current-item
  (save-excursion
    (forward-sexp 1)
    ;; (message "extra overlay at %d..%d" (1- (point)) (point))
    (versor-extra-overlay (1- (point)) (point)))) ; should probably be versor:add-to-current-item when I've written one

(defun versor-down-list (&rest args)
  (interactive "p")
  (apply 'down-list args)
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (make-versor-overlay (point) (1+ (point))) ; should probably be versor:set-current-item
  (when (looking-at "\\s(")
    (save-excursion
      (forward-sexp 1)
      ;; (message "extra overlay at %d..%d" (1- (point)) (point))
      (versor-extra-overlay (1- (point)) (point))))) ; should probably be versor:add-to-current-item when I've written one

;; left over from trying a window selection dimension
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

;; left over from a bumping character values dimension
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
    ;; (message "where=%S one-more=%S" where one-more)
    (if (and where
	     ;; one-more
	     )
	(progn
	  (goto-char where)
	  (let ((limit (save-excursion (last-sexp) (point))))
	    (when (> n 0)
	      (parse-partial-sexp where limit
				  0 t)))
	  (unless one-more
	    (versor:set-current-item where
				     (safe-scan-sexps (point) -1))))
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

;; left over from trying a buffer selection dimension
;; (defun next-buffer ()
;;   "Select the next buffer in this window."
;;   (interactive)
;;   (let ((this-buffer (current-buffer)))
;;     (switch-to-buffer (other-buffer this-buffer))
;;     (bury-buffer this-buffer)))
;; 
;; (defun previous-buffer ()
;;   "Select the previous buffer in this window."
;;   (interactive)
;;   (switch-to-buffer (car (last (buffer-list)))))
;; 
;; (defun last-buffer ()
;;   "Select the last buffer in this window."
;;   (interactive)
;; )
;; 
;; (defun first-buffer ()
;;   "Select the first buffer in this window."
;;   (interactive)
;; )

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
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-backward "<tr" (point-min) t)
	   (re-search-forward "<t[dh][^>]+" (point-max) t))
      t
    (error "Could not locate first cell.")))

(defun versor:previous-cell (n)
  "Move to the previous cell."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
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
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive "p")
  (while (> n 0)
    (if (not (re-search-forward "<t[dh][^>]+" (point-max) t))
	(error "No more next cells")
      (decf n))))

(defun versor:last-cell ()
  "Move to the last cell."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-forward "</tr" (point-max) t)
	   (re-search-backward "<t[dh][^>]+" (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last cell")))

(defun versor:first-row ()
  "Move to the first row."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-backward "<table" (point-min) t)
	   (re-search-forward "<tr[^>]+" (point-max) t))
      t
    (error "Could not locate first row")))

(defun versor:previous-row (n)
  "Move to the previous row."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
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
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive "p")
  (while (> n 0)
    (if (not (re-search-forward "<tr[^>]+" (point-max) t))
	(error "No more next rows")
      (decf n))))

(defun versor:last-row ()
  "Move to the last row."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-forward "</table" (point-max) t)
	   (re-search-backward "<tr[^>]+" (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last row")))

(defun versor:first-defun ()
  "Move to the start of the first function definition in the buffer."
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
  "Move to the start of the last function definition in the buffer."
  (interactive)
  (goto-char (point-max))
  (beginning-of-defun 1))

;;;; end of versor.el
