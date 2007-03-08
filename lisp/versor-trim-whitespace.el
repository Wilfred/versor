;;;; versor-trim-whitespace.el -- trim whitespace after a versor command
;;; Time-stamp: <2007-03-04 16:24:12 jcgs>

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

(defun versor-whitespace-preceding (where)
  "Return a description of the whitespace preceding WHERE.
This can be:
  'blank-line if there is a blank line there
  'start      if it is the start of the line
  'margin     if there is only whitespace as far as the start of line
  'space      if there is whitespace there
  nil         otherwise."
  (cond
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

(defun versor-whitespace-following (where)
  "Return a description of the whitespace following WHERE.
This can be:
  'blank-line if there is a blank line there
  'end        if it is the end of the line
  'margin     if there is only whitespace as far as the end of line
  'space      if there is whitespace there
  nil         otherwise."
  (cond
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
	 (preceding (versor-whitespace-preceding start))
	 (following (versor-whitespace-following end)))
    (put-text-property 0 len
		       'preceded-by
		       preceding
		       text)
    (put-text-property 0 len
		       'followed-by
		       following
		       text)
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
      (versor-adjust-whitespace (car ws) (cdr ws)))))

(defun versor-adjust-whitespace (neighbouring-to-string neighbouring-in-buffer)
  "Adjust whitespace after an insertion or deletion.
Each of the two arguments can be chosen from 'blank-line, 'end or
'start, 'margin, 'spaces, 'space, or nil. If NEIGHBOURING-TO-STRING and
NEIGHBOURING-IN-BUFFER are the same, make sure there is only one of them
in the buffer at point."
  (when versor-adjust-whitespace
    (if (eq neighbouring-in-buffer neighbouring-to-string)
	(cond
	 ((eq neighbouring-to-string 'space)
	  (just-one-space)))
      (cond
       ((eq neighbouring-to-string 'blank-line)
	(open-line 2))
       ((eq neighbouring-to-string 'margin)
	(newline-and-indent))
       ((or (eq neighbouring-to-string 'space)
	    (eq neighbouring-to-string 'spaces)) 
	(insert " "))))))

(defun versor-adjusting-insert (string)
  "Insert STRING.
Use its text properties 'preceded-by and 'followed-by, in conjunction with
the results of versor-whitespace-preceding and versor-whitespace-following
called at the insertion point, to adjust the whitespace around the insertion."
  (let ((preceding-in-string (get-text-property 0 'preceded-by string))
	(preceding-in-buffer (versor-whitespace-preceding (point)))
	(following-in-string (get-text-property (1- (length string))
						'followed-by string))
	(following-in-buffer (versor-whitespace-following (point))))
    (versor-adjust-whitespace preceding-in-string preceding-in-buffer)
    (insert string)
    (versor-adjust-whitespace following-in-string following-in-buffer)))

(provide 'versor-trim-whitespace)

;;; end of versor-trim-whitespace.el
