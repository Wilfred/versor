;;; versor-custom.el -- versatile cursor
;;; Time-stamp: <2006-04-10 12:21:06 john>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2006  John C. G. Sturdy
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

(provide 'versor-custom)

(defgroup versor nil
  "Switch cursor keys between different sorts of movement."
  :group 'editing
  :prefix "versor-")

(defvar versor-allow-move-to-end-of-last 'dwim
  "*Whether to allow moving to the end of the last sexp in a list.
  Otherwise, versor-next stops at the start of it, and refuses to do
  another forward move.
  Setting this non-nil does what you probably want in practice, although
  setting it nil is probably cleaner in some abstract sort of way.
  Setting it non-nil and not t will make the last move within a list
  go to just before the closing syntax of the list, which is where you
  typically want to be to type the next sexp in.")

(defvar versor-move-out-when-at-end t
  "If non-nil, trying to move further on when already at the end of
  the last thing in a container (see versor-allow-move-to-end-of-last)
  will move to just after the end of the container. Can be convenient in
  practice, although it breaks the symmetry of the next<-->previous
  operations.")

(defcustom versor-level-wrap t
  "*Whether to wrap the level changes"
  :group 'versor
  :type 'boolean)

(defcustom versor-meta-level-wrap t
  "*Whether to wrap the meta-level changes"
  :group 'versor
  :type 'boolean)

(defvar versor-multi-line-level-display (and (boundp 'emacs-major-version)
					     (>= emacs-major-version 21))
  "*Whether to use multi-line indication of the current meta-level and level.")


(defvar versor-highlight-with-brackets (not versor-use-face-attributes)
  "*Whether to use brackets around highlighted items in status feedback.")

(defcustom versor-change-cursor-color t
  "*Whether to use the cursor color to indicate the level."
  :group 'versor
  :type 'boolean)

(defcustom versor-item-attribute
  (if (< emacs-major-version 21)
      nil
    :background)
  "*An attribute to use to indicate the current item.
This is looked up in the current dimension, to get the value to set it to.
For example, if versor-item-attribute is :background, :background is looked
up in the current dimension, to find the colour to set the background to.
You can only use this from Emacs 21 onwards.")

(defcustom versor-try-to-display-whole-item t
  "*Whether to try to display the whole item after each movement."
  :group 'versor
  :type 'boolean)

(defcustom versor-reversible (not (eq window-system 'x))
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

;;;; some other variables

(defvar versor-use-face-attributes
  (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 21))
  "*Whether to use face attributes, as provided from Emacs 21 onwards.")

(defvar versor-mode-line-begin-string " <"
  "String corresponding to bracket for beginning of versor markers according to versor-reversed.")
(defvar versor-mode-line-end-string (if versor-reversible "==>" ">")
  "String corresponding to bracket for end of versor markers according to versor-reversed.")

(defvar versor-statement-insertion-with-dummy-value nil
  "*Whether versor statement insertion puts a placeholder value in when adding something.
This tries to avoid changing the semantics, for example, it uses
\"true\" when adding \"and\" or \"if\".
You can then change the value, using the versor alterations system.")

(defvar versor-per-buffer nil
  "*Whether to remember the dimensions per buffer")

(defvar versor-auto-change-for-modes t
  "*Whether to change the dimension on changing modes.")

(defvar versor-research-live-commentary nil
  "*Whether to make a live commentary on versor and other activities.")

(defvar versor-text-in-code t
  "*Whether versor should switch dimensions for string literals and comments.")

(defvar versor-text-faces '(font-lock-string-face font-lock-comment-face)
  "Faces which versor regards as being text rather than code.
See versor-text-in-code-function.")

(defvar versor-statement-up-to-next nil
  "Whether to make a statement extend all the way to the start of the next one.")

(defvar versor-item-face (make-face 'versor-item)
  "Face to use for versor items")

(when versor-use-face-attributes
  (set-face-attribute 'versor-item nil :inherit 'region))

(unless window-system
  (set-face-attribute 'versor-item nil :underline t))

(defvar choices-display-full t
  "*Whether to display in full the range of choices at each level of a treewise chooser.")

(defvar flexi-choose-upstring "[Up]"
  "The label to indicate going back up the tree.")

(defvar flexi-choose-topstring "[Top]"
  "The label to indicate going straight back up to the top of the tree.")

;;;; end of versor-custom.el
