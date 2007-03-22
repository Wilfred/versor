;;;; versor-trim-whitespace.el -- trim whitespace after a versor command
;;; Time-stamp: <2007-03-19 19:38:27 jcgs>

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

(defvar versor-trim-whitespace t
  "*Whether to adjust the whitespace around the place affected by each versor editing action.")

(defun versor-trim-whitespace (whereabouts)
  "Trim the whitespace around WHEREABOUTS."
  (when versor-trim-whitespace
    (condition-case evar
	(save-excursion
	  (goto-char whereabouts)
	  (let ((syntax-before (save-excursion
				 (skip-syntax-backward "-")
				 (char-syntax (char-before))))
		(syntax-after (save-excursion
				(skip-syntax-forward "-")
				(char-syntax (char-after)))))
	    (when (and syntax-before syntax-after)
	      (languide-trim-whitespace syntax-before syntax-after))))
      (error nil))))

(defun versor-what-is-preceding (where)
  "Return a description of the whitespace or other characters preceding WHERE.
This can be:
  'blank-line if there is a blank line there
  'start      if it is the start of the line
  'margin     if there is only whitespace as far as the start of line
  'space      if there is whitespace there
  'opening    if there is some kind of opening bracket there
  nil         otherwise."
  (cond
   ((save-excursion
      (goto-char (1- where))
      (looking-at "\\s("))
    'opening)
   ((save-excursion
      (goto-char where)
      (beginning-of-line 0)
      (looking-at "^\\s-*$"))
    'blank-line)
   ((bolp)
    'start)
   ((save-excursion
      (goto-char where)
      (skip-syntax-backward "-" (line-beginning-position))
      (bolp))
    'margin)
   ((save-excursion
      (goto-char (- where 2))
      (looking-at "\\s-"))
    'spaces)
   ((save-excursion
      (goto-char (1- where))
      (looking-at "\\s-"))
    'space)
   (t nil)))

(defun versor-what-is-following (where)
  "Return a description of the whitespace or other characters following WHERE.
This can be:
  'blank-line if there is a blank line there
  'end        if it is the end of the line
  'margin     if there is only whitespace as far as the end of line
  'space      if there is whitespace there
  'closing    if there is some kind of closing bracket there
  nil         otherwise."
  (cond
   ((save-excursion
      (goto-char where)
      (looking-at "\\s)"))
    'closing)
   ((save-excursion
      (goto-char where)
      (beginning-of-line 2)
      (looking-at "^\\s-*$"))
    'blank-line)
   ((eolp)
    'end)
   ((save-excursion
      (goto-char where)
      (skip-syntax-forward "-" (line-end-position))
      (eolp))
    'margin)
   ((save-excursion
      (goto-char (1+ where))
      (looking-at "\\s-"))
    'spaces)
   ((save-excursion
      (goto-char where)
      (looking-at "\\s-"))
    'space)
   (t nil)))

(defun versor-copy-region (start end)
  "Copy the region between START and END.
It is labelled with descriptions of the neighbouring whitespace.
The descriptions of the whitespace are returned, as a cons."
  (let* ((text (buffer-substring start end))
	 (len (length text))
	 (preceding (versor-what-is-preceding start))
	 (following (versor-what-is-following end)))
    (put-text-property 0 len
		       'preceded-by
		       preceding
		       text)
    (put-text-property 0 len
		       'followed-by
		       following
		       text)
    ;; (message "versor-copy-region %S..%S" preceding following)
    (if (eq last-command 'kill-region)
	(kill-append  text (< end start))
      (kill-new text))
    (cons preceding following)))

(defun versor-kill-region (start end)
  "Kill the region between START and END, labelling it with descriptions of the neighbouring whitespace."
  (let ((ws (versor-copy-region start end)))
    (delete-region start end)
    (when versor-adjust-whitespace
      ;; todo: suspend this until we move away from here, so that if
      ;; we start typing, the space is as it was?
      ;; (message "versor-kill-region %S..%S" (car ws) (cdr ws))
      (versor-adjust-whitespace (car ws) (cdr ws) " around deleted text"))))

(defvar versor-can-do-delayed-deletions t
  "Whether we can currently do delayed deletions.")

(defvar versor-delayed-deletions nil
  "Things pending deletion resulting from versor-delayed-delete.")

(make-variable-buffer-local 'versor-delayed-deletions)

(defvar versor-cancelling-deledendum nil)

(defun versor-delete-deledendum (old new)
  "point-left hook function for versor-delayed-delete."
  (when versor-can-do-delayed-deletions
    (let ((versor-cancelling-deledendum t))
      (message "Doing delayed deletions")
      (backtrace)
      (mapcar (lambda (pair)
		(message "  deleting %d..%d" (car pair) (cdr pair))
		(delete-region (car pair) (cdr pair)))
	      versor-delayed-deletions)
      (setq versor-delayed-deletions nil))))

(defun versor-cancel-deledendum (from to)
  "modification-hooks hook function for versor-delayed-delete."
  (when (and versor-can-do-delayed-deletions
	     (not versor-cancelling-deledendum))
    (let ((versor-cancelling-deledendum t))
      (message "Cancelling delayed deletions")
      (mapcar (lambda (pair)
		(message "  cancelling %d..%d" (car pair) (cdr pair))
		(remove-text-properties (car pair) (cdr pair)
					'(point-left nil
						     'modification-hooks nil
						     'face nil)))
	      versor-delayed-deletions)
      (setq versor-delayed-deletions nil))))

(defun versor-delayed-delete (start end)
  "Delete the text between START and END when point moves out of it.
If a change happens before point moves out of it, don't delete it."
  (if (or (< (point) start)
	  (>= (point) end))
      (delete-region start end)
    (let ((versor-can-do-delayed-deletions nil))
      (message "arranging delayed deletion for %d..%d" start end)
      (push (cons start end) versor-delayed-deletions)
      (put-text-property start end 'face (cons 'background-color "red"))
      (put-text-property start end 'point-left 'versor-delete-deledendum)
      (put-text-property start end 'modification-hooks
			 (cons 'versor-cancel-deledendum
			       (get-text-property start
						  'modification-hooks)))
      (goto-char start))))

(defun versor-delayed-delete-horizontal-space ()
  "Like delete-horizontal-space, but using versor-delayed-delete."
  (let ((space-start (save-excursion (skip-syntax-backward "-") (point)))
	(space-end (save-excursion (skip-syntax-forward "-") (point))))
    (versor-delayed-delete space-start space-end)))

(defun versor-adjust-whitespace (neighbouring-a neighbouring-b &optional debug-label)
  "Adjust whitespace after an insertion or deletion.
Each of the two arguments can be chosen from 'blank-line, 'end or
'start, 'margin, 'spaces, 'space, 'closing or 'opening, or nil. If
NEIGHBOURING-A and NEIGHBOURING-B are the same, make
sure there is only one of them in the buffer at point."
  (when versor-adjust-whitespace
    ;; (message "versor-adjust-whitespace%s neighbouring-a=%S neighbouring-b=%S" (if debug-label debug-label "") neighbouring-a neighbouring-b)
    (let ((versor-can-do-delayed-deletions nil))
      (if (eq neighbouring-b neighbouring-a)
	  (cond
	   ((eq neighbouring-a 'space)
	    ;; (message "both were spaces, leaving one space")
	    (just-one-space))
	   ((eq neighbouring-a 'margin)
	    (delete-blank-lines))
	   ((eq neighbouring-a 'blank-line)
	    (delete-blank-lines)
	    (delete-blank-lines)
	    (open-line 1)))
	(cond
	 ((eq neighbouring-a 'opening)
	  ;; (message "neighbouring-a was opening, closing up")
	  (versor-delayed-delete-horizontal-space))
	 ((eq neighbouring-b 'closing)
	  ;; (message "neighbouring-b was closing, closing up")
	  (versor-delayed-delete-horizontal-space))
	 ((eq neighbouring-a 'blank-line)
	  ;; (message "neighbour-a was blank line, opening line")
	  (delete-blank-lines)
	  (delete-blank-lines)
	  (open-line 1))
	 ((eq neighbouring-a 'margin)
	  ;; todo: don't always want to do this
	  ;; (message "neighbour-a was margin, newline-and-indent")
	  (newline-and-indent))
	 ((or (eq neighbouring-a 'space)
	      (eq neighbouring-a 'spaces))
	  ;; (message "neighbour-a was whitespace, inserting space")
	  (insert " ")))))))

(defun versor-adjusting-insert (string)
  "Insert STRING.
Use its text properties 'preceded-by and 'followed-by, in conjunction with
the results of versor-what-is-preceding and versor-what-is-following
called at the insertion point, to adjust the whitespace around the insertion.
Returns a cons of the start and end positions of where STRING itself
was inserted."
  (let ((preceding-in-string (get-text-property 0 'preceded-by string))
	(preceding-in-buffer (versor-what-is-preceding (point)))
	(following-in-string (get-text-property (1- (length string))
						'followed-by string))
	(following-in-buffer (versor-what-is-following (point))))
    ;; (message "versor-adjusting-insert %S:\"%S ... %S\":%S" preceding-in-buffer preceding-in-string following-in-string following-in-buffer)
    (versor-adjust-whitespace preceding-in-string preceding-in-buffer " before inserted text")
    (let ((start (point)))
      (insert string)
      (let ((end (point)))
	(versor-adjust-whitespace following-in-string following-in-buffer " after inserted text")
	(cons start end)))))

(provide 'versor-trim-whitespace)

;;; end of versor-trim-whitespace.el
