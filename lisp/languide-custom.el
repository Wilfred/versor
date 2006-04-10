;;;; languide-custom.el -- customization definitions for languide
;;; Time-stamp: <2006-04-10 10:12:13 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'languide-custom)

(defgroup languide nil
  "Languide-guided editing."
  :group 'editing
  :prefix "languide-")

(defvar languide-auto-edit-overlay-face (cons 'background-color "red")
  "How to draw attention to what languide has done.")

(defvar languide:debug-messages nil)
(defvar debug-functions '(
			 ;;  beginning-of-statement-internal
			  ;; end-of-statement-internal
			  ;; continue-back-past-curly-ket
			  ;; previous-statement
			  ;; next-statement
			  ;; navigate-to
			  ))


(defvar navigate-container-whole-statement t
  "*Whether to include the whole statement as the container.")

(defvar statement-navigate-parts-cyclic nil
  "*Whether to step forwards from body (or tail if present) back round to head.")

(defvar statement-navigate-parts-include-container t
  "*Whether to step forwards from body (or tail if present) or back from head, to container.")

(defvar languide-parts '("container" "framework" "whole" "head" "body" "tail")
  "The parts we can navigate to.")

;;; end of languide-custom.el
