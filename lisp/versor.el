;;; versor.el -- versatile cursor
;;; Time-stamp: <2004-05-24 09:36:43 john>
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
			;; (delete delete-char)
			))

(versor:define-moves movemap-lines
		     '((color "black")
		       (first beginning-of-buffer)
		       (previous versor:previous-line)
		       (next versor:next-line)
		       (last end-of-buffer)
		       (end-of-item end-of-line)
		       ;; (delete kill-line)
		       ))

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
		       ;; (delete kill-sexp)
		       ))

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
		       (last end-of-defun) ;;;;;;;;;;;;;;;; make this go back one statement from the end of the defun
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
