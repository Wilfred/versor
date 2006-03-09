;;;; versor-trim-whitespace.el -- trim whitespace after a versor command
;;; Time-stamp: <2006-03-09 14:52:36 john>

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

;;; Some of my keystrokes go into adjusting the whitespace after
;;; deleting or inserting something. I think the adjustment is
;;; completely predicatable, so I'll try getting emacs to do the work
;;; for me. The end of a versor editing command is often the right
;;; time to adjust whitespace, hence doing it this way. I doubt it
;;; would make a good general post-command-hook!

(provide 'versor-trim-whitespace)

(defvar versor-trim-whitespace t
  "*Whether to adjust the whitespace around the place affected by each versor editing action.")

(defun versor-trim-whitespace (whereabouts)
  "Trim the whitespace around WHEREABOUTS."
  (when versor-trim-whitespace
    (save-excursion
      (goto-char whereabouts)
      (let ((syntax-before (save-excursion
			     (skip-syntax-backward "-")
			     (char-syntax (char-before))))
	    (syntax-after (save-excursion
			    (skip-syntax-forward "-")
			    (char-syntax (char-after)))))
	(message "%c %c" syntax-before syntax-after)
	(languide-trim-whitespace syntax-before syntax-after)))))

;;; end of versor-trim-whitespace.el
