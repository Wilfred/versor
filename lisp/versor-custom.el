;;; versor-custom.el -- versatile cursor
;;; Time-stamp: <2004-05-24 15:45:07 john>
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

(provide 'versor-custom)

(defgroup versor nil
  "Switch cursor keys between different sorts of movement."
  :version "0.1"
  :group 'editing
  :prefix "versor:")

(defcustom versor:level-wrap t
  "*Whether to wrap the level changes"
  :group 'versor
  :type 'boolean)

(defcustom versor:meta-level-wrap t
  "*Whether to wrap the meta-level changes"
  :group 'versor
  :type 'boolean)

(defcustom versor:change-cursor-color t
  "*Whether to use the cursor color to indicate the level."
  :group 'versor
  :type 'boolean)

(defcustom versor:item-attribute nil
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

;;;; end of versor-custom.el
