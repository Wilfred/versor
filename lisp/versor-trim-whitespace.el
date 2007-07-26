;;;; versor-trim-whitespace.el -- trim whitespace after a versor command
;;; Time-stamp: <2007-07-25 20:16:23 jcgs>

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

;;; todo: possibly use insert-for-yank?

(defvar versor-trim-whitespace t
  "*Whether to adjust the whitespace around the place affected by each versor editing action.")

(defun versor-trim-whitespace (whereabouts)
  "Trim the whitespace around WHEREABOUTS."
  ;; Used from versor-yank (commented out),
  ;; languide-remove-surrounding-call. Is this function obsolescent?  I
  ;; think it's all moving towards versor-copy-region,
  ;; versor-kill-region, versor-adjust-whitespace, and
  ;; versor-adjusting-insert.
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
  (save-excursion
    (cond
     ((progn
	(goto-char (1- where))
	(looking-at "\\s("))
      'opening)
     ((progn
	(goto-char where)
	(beginning-of-line 0)
	(looking-at "^\\s-*$"))
      'blank-line)
     ((progn
	(goto-char where)
	(bolp))
      'start)
     ((progn
	(goto-char where)
	(skip-syntax-backward "-" (line-beginning-position))
	(bolp))
      'margin)
     ((progn
	(goto-char (- where 2))
	(looking-at "\\s-"))
      'spaces)
     ((progn
	(goto-char (1- where))
	(looking-at "\\s-"))
      'space)
     (t nil))))

(defun versor-what-is-following (where)
  "Return a description of the whitespace or other characters following WHERE.
This can be:
  'blank-line if there is a blank line there
  'end        if it is the end of the line
  'margin     if there is only whitespace as far as the end of line
  'space      if there is whitespace there
  'closing    if there is some kind of closing bracket there
  nil         otherwise."
  (save-excursion
    (cond
     ((progn
	(goto-char where)
	(looking-at "\\s)"))
      'closing)
     ((progn
	(goto-char where)
	(beginning-of-line 2)
	(looking-at "^\\s-*$"))
      'blank-line)
     ((progn
	(goto-char where)
	(eolp))
      'end)
     ((progn
	(goto-char where)
	(skip-syntax-forward "-" (line-end-position))
	(eolp))
      'margin)
     ((progn
	(goto-char (1+ where))
	(looking-at "\\s-"))
      'spaces)
     ((progn
	(goto-char where)
	(looking-at "\\s-"))
      'space)
     (t nil))))

(defvar versor-debug-adjust-whitespace t
  "*Whether to say what's going on with the whitespace adjustment.")

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
    (when versor-debug-adjust-whitespace
      (message "versor-copy-region %S..%S..%S" preceding text following))
    (if (eq last-command 'kill-region)
	(kill-append  text (< end start))
      (kill-new text))
    (if (fboundp 'update-shown-stacks)	; from rpn-edit.el
	(update-shown-stacks))
    (cons preceding following)))

(defun versor-kill-region (start end)
  "Kill the region between START and END, labelling it with descriptions of the neighbouring whitespace."
  (let ((ws (versor-copy-region start end)))
    (delete-region start end)
    (when versor-adjust-whitespace
      (when versor-debug-adjust-whitespace
	(message "versor-kill-region %S..%S" (car ws) (cdr ws)))
      (versor-adjust-whitespace start
				(car ws) (cdr ws)
				" around deleted text"))))

(defvar versor-can-do-delayed-deletions t
  "Whether we can currently do delayed deletions.
This is not meant to be set; it gets bound to nil within some functions,
to hold off the delayed deletions.  Use versor-delay-deletions to control
whether deletions are delayed.")

(defvar versor-delayed-deletions nil
  "Things pending deletion resulting from versor-delayed-delete.
Each item is a cons of a cons of start and end, and an optional insertion.")

(make-variable-buffer-local 'versor-delayed-deletions)

(defvar versor-cancelling-deledendum nil)

(defun versor-delete-deledendum (old new)
  "point-left hook function for versor-delayed-delete."
  (when versor-can-do-delayed-deletions
    (let ((versor-cancelling-deledendum t))
      (message "Doing delayed deletions")
      (backtrace)
      (mapcar (lambda (update)
		(message "  delayed: deleting %d..%d" (caar update) (cdar update))
		(delete-region (caar update) (cdar update))
		(when (cdr update)
		  (message "  delayed: inserting %S" (cdr update))
		  (goto-char (caar update))
		  (insert (cdr update))))
	      versor-delayed-deletions)
      (setq versor-delayed-deletions nil))))

(defun versor-cancel-deledendum (from to)
  "modification-hooks hook function for versor-delayed-delete."
  (when (and versor-can-do-delayed-deletions
	     (not versor-cancelling-deledendum))
    (let ((versor-cancelling-deledendum t))
      (message "Cancelling delayed deletions")
      (mapcar (lambda (update)
		(message "  cancelling %d..%d" (caar update) (cdar update))
		(remove-text-properties (caar update) (cdar update)
					'(point-left nil
						     modification-hooks nil
						     face nil)))
	      versor-delayed-deletions)
      (setq versor-delayed-deletions nil))))

(defun versor-delayed-delete (start end &optional insertion)
  "Delete the text between START and END when point moves out of it.
With optional INSERTION, insert that after the deletion.
If a change happens before point moves out of it, don't delete it.
If versor-delay-deletions is nil, just do an ordinary deletion immediately."
  (message "versor-delayed-delete %d..%d %S" start end insertion)
  (if versor-delay-deletions
      (if (and nil
	       (or (< (point) start)
		   (>= (point) end)))
	  (delete-region start end)
	(let ((versor-can-do-delayed-deletions nil))
	  (message "arranging delayed deletion for %d..%d" start end)
	  (push (cons (cons start
			    end)
		      insertion)
		versor-delayed-deletions)
	  (put-text-property start end 'face (cons 'background-color "red"))
	  ;; todo: set fringes as well
	  (put-text-property start end 'point-left 'versor-delete-deledendum)
	  (put-text-property start end 'modification-hooks
			     (cons 'versor-cancel-deledendum
				   (get-text-property start
						      'modification-hooks)))
	  (goto-char start)))
    (message "deleting immediately %d..%d" start end)
    (delete-region start end)
    (when insertion
      (message "inserting immediately %d %S" start insertion)
      (goto-char start)
      (insert insertion))))

(defun versor-delayed-delete-horizontal-space (place)
  "Like delete-horizontal-space at PLACE, but using versor-delayed-delete."
  (let ((space-start (save-excursion (goto-char place)
				     (skip-syntax-backward "-")
				     (point)))
	(space-end (save-excursion (goto-char place)
				   (skip-syntax-forward "-")
				   (point))))
    (versor-delayed-delete space-start space-end)))

(defun versor-delayed-one-blank-line-at (place)
  "Leave one blank line at PLACE.
If versor-delay-deletions is non-nil, don't actually do it until
point leaves the text that would be deleted; and cancel it if the
user makes changes in that text."
  ;; todo: use versor-delayed-delete
  (save-excursion
    (message "versor-delayed-one-blank-line-at %d" (point))
    (goto-char place)
    (skip-syntax-backward "->")
    (let ((start (point)))
      (skip-syntax-forward "->")
      (versor-delayed-delete start (point) "\n\n"))))

(defun versor-delayed-just-one-space (place)
  "Leave one space at PLACE.
If versor-delay-deletions is non-nil, don't actually do it until
point leaves the text that would be deleted; and cancel it if the
user makes changes in that text."
  ;; todo: use versor-delayed-delete
  (goto-char place)
  (just-one-space))

(defun versor-delayed-delete-blank-lines (place)
  "Delete blank lines around PLACE.
If versor-delay-deletions is non-nil, don't actually do it until
point leaves the text that would be deleted; and cancel it if the
user makes changes in that text."
  ;; todo: use versor-delayed-delete
  (goto-char place)
  (delete-blank-lines))

(defun versor-delayed-newline-and-indent (place)
  "Like newline-and-indent at PLACE, but delayed."
  ;; todo: use versor-delayed-delete
  (goto-char place)
  (newline-and-indent))

(defun versor-adjust-whitespace (around neighbouring-a neighbouring-b &optional debug-label)
  "Adjust whitespace after an insertion or deletion.
The first argument is where the edit occurred.
Each of the following two arguments can be chosen from 'blank-line,
'end or 'start, 'margin, 'spaces, 'space, 'closing or 'opening, or
nil. If NEIGHBOURING-A and NEIGHBOURING-B are the same, make sure
there is only one of whatever they are in the buffer at point.
When deleting a piece of text, NEIGHBOURING-A describes the text
just before the deleted piece, and NEIGHBOURING-B the text just
after it.
When inserting text, versor-adjust-whitespace is called twice,
first with NEIGHBOURING-A being describing the text before the
insertion, and NEIGHBOURING-B describing the start of the
insertion; then with NEIGHBOURING-A describing the end of the
insertion, and NEIGHBOURING-B describing the text just after the
insertion."
  (when versor-adjust-whitespace
    (when versor-debug-adjust-whitespace
      (message "")
      (message "versor-adjust-whitespace%s at %s, neighbouring-a=%S neighbouring-b=%S"
	       (if debug-label debug-label "")
	       (point)
	       neighbouring-a
	       neighbouring-b))
    (save-excursion
      (goto-char around)
      (let ((versor-can-do-delayed-deletions nil))
	(if (eq neighbouring-b neighbouring-a)
	    (cond
	     ((eq neighbouring-a 'space)
	      (when versor-debug-adjust-whitespace
		(message "both were spaces, leaving one space"))
	      (versor-delayed-just-one-space around))
	     ((eq neighbouring-a 'margin)
	      (when versor-debug-adjust-whitespace
		(message "both were in margin, deleting blank lines"))
	      (versor-delayed-delete-blank-lines around))
	     ((eq neighbouring-a 'blank-line)
	      (when versor-debug-adjust-whitespace
		(message "both were blank lines, leaving one blank line"))
	      (versor-delayed-one-blank-line-at around)))
	  (cond
	   ((eq neighbouring-a 'opening)
	    (when versor-debug-adjust-whitespace
	      (message "neighbouring-a was opening, closing up"))
	    (versor-delayed-delete-horizontal-space around))
	   ((eq neighbouring-b 'closing)
	    (when versor-debug-adjust-whitespace
	      (message "neighbouring-b was closing, closing up"))
	    (versor-delayed-delete-horizontal-space around))
	   ((eq neighbouring-a 'blank-line)
	    (when versor-debug-adjust-whitespace
	      (message "neighbour-a was blank line, opening line at %d" around))
	    (versor-delayed-one-blank-line-at around))
	   ((eq neighbouring-a 'margin)
	    ;; todo: don't always want to do this
	    (when versor-debug-adjust-whitespace
	      (message "neighbour-a was margin, newline-and-indent at %d" (point)))
	    (versor-delayed-newline-and-indent around))
	   ((or (eq neighbouring-a 'space)
		(eq neighbouring-a 'spaces))
	    (when versor-debug-adjust-whitespace
	      (message "neighbour-a was whitespace, inserting space at %d" around))
	    (versor-delayed-just-one-space around))))))))

(defun versor-adjusting-insert (string)
  "Insert STRING.
Use its text properties 'preceded-by and 'followed-by, in conjunction with
the results of versor-what-is-preceding and versor-what-is-following
called at the insertion point, to adjust the whitespace around the insertion.
STRING may be a single string, or a cons of two strings. In the
latter case, the car of them is inserted first, the variable
versor-mid-insertion-place gets set to point, and then then cdr
of them is inserted. (Otherwise, versor-mid-insertion-place is nil.)
Returns a cons of the start and end positions of where STRING itself
was inserted."
  (setq versor-mid-insertion-place nil)
  (let ((preceding-in-string (get-text-property
			      0
			      'preceded-by
			      (if (stringp string)
				  string
				(car string))))
	(preceding-in-buffer (versor-what-is-preceding (point)))
	(following-in-string (if (stringp string)
				 (get-text-property (1- (length string))
						    'followed-by string)
			       (get-text-property (1- (length (cdr string)))
						  'followed-by (cdr string))))
	(following-in-buffer (versor-what-is-following (point))))
    (when versor-debug-adjust-whitespace
      (message "versor-adjusting-insert %S:\"%S ... %S\":%S" 
	       preceding-in-buffer preceding-in-string 
	       following-in-string following-in-buffer))
    (let ((start (point)))
      (versor-adjust-whitespace start
				preceding-in-string
				preceding-in-buffer
				" before inserted text")
      (if (stringp string)
	  (insert string)
	(insert (car string))
	(setq versor-mid-insertion-place (point))
	(insert (cdr string)))
      (let ((end (point)))
	(versor-adjust-whitespace end
				  following-in-string
				  following-in-buffer
				  " after inserted text")
	(cons start end)))))

(provide 'versor-trim-whitespace)

;;; end of versor-trim-whitespace.el
