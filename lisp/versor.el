;;; versor.el -- versatile cursor
;;; Time-stamp: <2006-05-04 19:01:52 john>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006  John C. G. Sturdy
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
(require 'versor-custom)
(require 'versor-dimensions)
(require 'versor-status)
(require 'versor-selection)
(require 'versor-base-moves)
(require 'versor-commands)
(require 'versor-alter-item)		; should probably autoload
(require 'versor-containers)		; should probably autoload
(require 'versor-chop-chop)		; should probably autoload
(require 'versor-dwim)			; should probably autoload

;; todo: command to move to end of container (possibly improved semantics for versor-end)
;; todo: command to toggle between code and string literals
;; todo: fix change of dimensions that happens after type-break uses the minibuffer (probably more general than this)
;; todo: make mouse clicks and drags select and extend using the current versor dimensions
;; todo: use custom
;; todo: use Info-file-list-for-emacs

(defvar version-version "1.0"
  "The version number for this release of versor.")

;;;;;;;;;;;;;;;
;;;; hooks ;;;;
;;;;;;;;;;;;;;;

(defvar versor-start-hooks nil
  "*Hooks for versor-start.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor-prev-hooks nil
  "*Hooks for versor-prev.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor-next-hooks nil
  "*Hooks for versor-next.
If one of these returns non-nil, it is taken as having done the action.")

(defvar versor-end-hooks nil
  "*Hooks for versor-end.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autoload for demo ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'versor-demo "versor-demo"
  "Demonstrate versor." t)

;;;;;;;;;;;;;;;
;;;; Setup ;;;;
;;;;;;;;;;;;;;;

;; To use versor-
;;   add the directory containing this elisp file, and its companions, to your load-path
;;   load this file (or autoload versor-setup)
;;   call versor-setup
;; If you want to use navigation by statements, you will need languide too.

(defvar versor-insertion-placement-keymap (make-sparse-keymap "Versor insert place")
  "Keymap for reading what place to do insertion at.
This is for choosing before, after, around or inside.")

(fset 'versor-insertion-placement-keymap versor-insertion-placement-keymap)

(defvar versor-altering-map (make-sparse-keymap "Versor alter item")
  "Keymap for altering the selected item.")

(fset 'versor-altering-map versor-altering-map)

(defun versor-setup (&rest keysets)
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
to select which keys are set up to do versor commands.

You can turn on further facilities by including the following symbols
amongst the arguments:
  'modal         -- remember a different dimension for each mode
  'local         -- remember a different dimension for each buffer
  'text-in-code  -- switch dimensions for string literals and comments,
                    allowing code-oriented movement in actual code, and
                    text-oriented movement in embedded natural language text
  'menu          -- define a menu of versor commands
  'verbose       -- blather about what it is doing
"
  (interactive)

  (when (null keysets) (setq keysets '(arrows arrows-misc)))

  ;; todo: add setting of versor-meta-dimensions-valid-for-modes, and add to info file, and document this

  (when (memq 'modal keysets)
    ;; (require 'versor-modal)
    (require 'versor-local)
    (setq versor-auto-change-for-modes t))

  (when (memq 'local keysets)
    (require 'versor-local)
    (setq versor-per-buffer t))

  (when (memq 'text-in-code keysets)
    (require 'versor-text-in-code)
    (setq versor-text-in-code t))

  (when (memq 'menu keysets)
    (require 'versor-menu))

  (when (memq 'research keysets)
    (require 'versor-research)
    (versor-research-start))

  (when (memq 'verbose keysets)
    (setq versor-verbose t))

  (when (memq 'arrows keysets)
    (global-set-key [ left ]    'versor-prev)
    (global-set-key [ right ]   'versor-next)
    (global-set-key [ M-left ]  'versor-out)
    (global-set-key [ M-right ] 'versor-in)
    (global-set-key [ C-left ]  'versor-extend-item-backwards)
    (global-set-key [ C-right ] 'versor-extend-item-forwards)

    (global-set-key [ up ]      'versor-over-prev)
    (global-set-key [ down ]    'versor-over-next)
    (global-set-key [ M-up ]    'versor-prev-meta-level)
    (global-set-key [ M-down ]  'versor-next-meta-level)
    (global-set-key [ C-up ]    'versor-over-start)
    (global-set-key [ C-down ]  'versor-over-end)

    (define-key versor-altering-map [ left ] 'versor-alter-item-prev)
    (define-key versor-altering-map [ right ] 'versor-alter-item-next)
    (define-key versor-altering-map [ up ] 'versor-alter-item-over-prev)
    (define-key versor-altering-map [ down ] 'versor-alter-item-over-next)
    )

  (when (memq 'arrows-misc keysets)
    ;; todo: I think I can make better use of these -- perhaps put versor-begin-altering-item and versor-dwim on them?
    (require 'languide-keymap)		; I don't think you can autoload keymaps, but perhaps I should try?
    (global-set-key [ prior ]   'versor-over-over-prev)
    (global-set-key [ next ]    'versor-over-over-next)
    (global-set-key [ C-prior ] 'versor-over-over-start)
    (global-set-key [ C-next ]  'versor-over-over-end)

    (global-set-key [ home ]    'versor-start)
    (global-set-key [ end ]     'versor-end)
    (global-set-key [ C-home ]  'versor-start-of-item)
    (global-set-key [ C-end ]   'versor-end-of-item)

    (global-set-key [ M-home ] 'versor-dwim)    

    ;; todo: why isn't this a normal global-set-key?
    (define-key (current-global-map) [ insert ]   'versor-insertion-placement-keymap)
    (global-set-key [ M-insert ] 'versor-begin-altering-item)
    (define-key global-map [ C-insert ] 'languide-map)
    (define-key versor-altering-map [ insert ] 'versor-end-altering-item)

    (global-set-key [ delete ]   'versor-kill)
    (global-set-key [ M-delete ] 'versor-copy)
    (define-key versor-altering-map [ delete ] 'versor-abandon-altering-item)

    (define-key versor-insertion-placement-keymap [ left ]  'versor-insert-before)
    (define-key versor-insertion-placement-keymap [ right ] 'versor-insert-after)
    (define-key versor-insertion-placement-keymap [ up ]    'versor-insert-around)
    (define-key versor-insertion-placement-keymap [ down ]  'versor-insert-within)
    )

  (when (memq 'keypad keysets)
    (global-set-key [ kp-left ]    'versor-prev)
    (global-set-key [ kp-right ]   'versor-next)
    (global-set-key [ M-kp-left ]  'versor-out)
    (global-set-key [ M-kp-right ] 'versor-in)
    (global-set-key [ C-kp-left ]  'versor-start)
    (global-set-key [ C-kp-right ] 'versor-end)
    (global-set-key [ kp-home ]    'versor-start)
    (global-set-key [ kp-end ]     'versor-end)
    (global-set-key [ C-kp-home ]  'versor-start-of-item)
    (global-set-key [ C-kp-end ]   'versor-end-of-item)
    (global-set-key [ M-kp-home ] 'versor-dwim)

    (define-key versor-altering-map [ kp-left ] 'versor-alter-item-prev)
    (define-key versor-altering-map [ kp-right ] 'versor-alter-item-next)
  
    (global-set-key [ kp-up ]      'versor-over-prev)
    (global-set-key [ kp-down ]    'versor-over-next)
    (global-set-key [ M-kp-up ]    'versor-prev-meta-level)
    (global-set-key [ M-kp-down ]  'versor-next-meta-level)
    (global-set-key [ C-kp-up ]    'versor-over-start)
    (global-set-key [ C-kp-down ]  'versor-over-end)

    (define-key versor-altering-map [ kp-up ] 'versor-alter-item-over-prev)
    (define-key versor-altering-map [ kp-down ] 'versor-alter-item-over-next)

    (global-set-key [ kp-prior ]   'versor-over-over-prev)
    (global-set-key [ kp-next ]    'versor-over-over-next)
    (global-set-key [ C-kp-prior ] 'versor-over-over-start)
    (global-set-key [ C-kp-next ]  'versor-over-over-end))

  (when (memq 'keypad-misc keysets)
    (global-set-key [ kp-enter ]  'versor-copy)
    (define-key (current-global-map)
                    [ kp-insert ] 'versor-insertion-placement-keymap)
    (global-set-key [ kp-delete ] 'versor-kill)
    (global-set-key [ kp-add ]    'other-window)

    (define-key (current-global-map) [ C-kp-delete ]   'versor-begin-altering-item)
    (define-key global-map [ C-kp-insert ] 'languide-map)

    (define-key versor-insertion-placement-keymap [ kp-left ]  'versor-insert-before)
    (define-key versor-insertion-placement-keymap [ kp-right ] 'versor-insert-after)
    (define-key versor-insertion-placement-keymap [ kp-up ]    'versor-insert-around)
    (define-key versor-insertion-placement-keymap [ kp-down ]  'versor-insert-within)
    )
  
  (unless (memq 'versor-current-level-name global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(""		; needed to stop fancy control actions
					; in the rest of this list
		    versor-mode-line-begin-string
		    versor-current-meta-level-name ":"
		    versor-current-level-name
		    versor-mode-line-end-string))))
  
  (versor-set-status-display))

(defun versor-speak (format-string &rest args)
  "If versor-speaking is non-nil, say FORMAT-STRING, using it to format ARGS."
  (when versor-speaking
    (dtk-speak (apply 'format format-string args))))

;;;; end of versor.el
