;;; joystick-minibuf.el --- enter text using the joystick

;; Copyright (C) 2007  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: hardware

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Text entry using the joystick hat switch, like on some GPS units.
;;
;; (I haven't yet done a patent search for this; I'm pretty sure the
;; idea is ancient and general, and also I've made this much nicer for
;; when choosing from a known set of choices.)
;;
;; Laborious, but for a couple of letters could be worth doing if you
;; don't want to move your hands from joystick to keyboard; and might
;; be useful for people with limited dexterity (possibly with a
;; specially-built hat switch) .
;;
;; There are two forms of entry supported:
;;
;; (1) Non-completing, in which "up" and "down" just bump the
;;     character at point to the next possible character, and "right"
;;     moves you to the next character position (or inserts a letter
;;     half-way through the alphabet if it was blank)
;; (2) Completing, in which "up" and "down" move you to the next
;;     letter that has a valid completion at that point, and "right"
;;     moves you to the next point at which completions differ.
;;
;; I've had another idea for text entry using a gamepad: since there
;; are two mini-joysticks, each with eight easily-recognized octants,
;; it should be possible to enter 8*8 = 64 possible characters using
;; them.  I may well write this soon.

;;; Code:

(require 'joystick)

(defun joystick-char-increment ()
  "Increment the character at point."
  (interactive)
  (let ((c (char-after (point))))
    (delete-char 1)
    (insert (1+ c))
    (backward-char 1)))

(defun joystick-char-decrement ()
  "Decrement the character at point."
  (interactive)
  (let ((c (char-after (point))))
    (delete-char 1)
    (insert (1- c))
    (backward-char 1)))

(defun end-of-class (char)
  "Return the last character of the same kind as CHAR."
  (cond
   ((and (<= ?a char)
	 (<= char ?z))
    ?z)
   ((and (<= ?A char)
	 (<= char ?Z))
    ?Z)
   ((and (<= ?0 char)
	 (<= char ?9))
    ?9)
   (t ?~)))

(defun start-of-class (char)
  "Return the first character of the same kind as CHAR."
  (cond
   ((and (<= ?a char)
	 (<= char ?z))
    ?a)
   ((and (<= ?A char)
	 (<= char ?Z))
    ?A)
   ((and (<= ?0 char)
	 (<= char ?9))
    ?0)
   (t ?!)))

(defun joystick-minibuffer-possibilities (pattern)
  "Return the possibilities matching PATTERN."
  (let* ((completion-regexp-list (list pattern))
	 (possibilities
	  (sort (all-completions contents
				 minibuffer-completion-table
				 minibuffer-completion-predicate
				 t)
		'string<)))
    possibilities))

(defun possibilities-below (original-char contents)
  "Helper function for joystick-valid-char-decrement."
  (nreverse
   (catch 'done
     (let ((possibilities (joystick-minibuffer-possibilities
			   (format "^%s[-%c-%c]"
				   contents
				   (start-of-class original-char)
				   (1- original-char))))
	   (ttl 4))
       ;; if we didn't get any in this class, try the other classes
       (while (null possibilities)
	 (when (zerop (setq ttl (1- ttl)))
	   (throw 'done nil))
	 (setq original-char
	       (cond
		((and (<= ?a original-char)
		      (<= original-char ?z))
		 ?Z)
		((and (<= ?A original-char)
		      (<= original-char ?Z))
		 ?9)
		((and (<= ?0 original-char)
		      (<= original-char ?9))
		 ?z)))
	 (setq possibilities (joystick-minibuffer-possibilities
			      (format "^%s[-%c-%c]"
				      contents
				      (1+ original-char)
				      (end-of-class original-char)))))
       possibilities))))

(defun possibilities-above (original-char contents)
  "Helper function for joystick-valid-char-increment."
  (catch 'done
    (let ((possibilities (joystick-minibuffer-possibilities
			  (format "^%s[-%c-%c]"
				  contents
				  (1+ original-char)
				  (end-of-class original-char))))
	  (ttl 4))
      ;; if we didn't get any in this class, try the other classes
      (while (null possibilities)
	(when (zerop (setq ttl (1- ttl)))
	  (throw 'done nil))
	(setq original-char
	      (cond
	       ((and (<= ?a original-char)
		     (<= original-char ?z))
		?0)
	       ((and (<= ?A original-char)
		     (<= original-char ?Z))
		?a)
	       ((and (<= ?0 original-char)
		     (<= original-char ?9))
		?A)))
	(setq possibilities (joystick-minibuffer-possibilities
			     (format "^%s[-%c-%c]"
				     contents
				     (1+ original-char)
				     (end-of-class original-char)))))
      possibilities)))

(defun joystick-valid-char-increment ()
  "Increment the character at point, using completion."
  (interactive)
  (unless (minibufferp)
    (error "Command joystick-valid-char-decrement only valid in minibuffer"))
  (let* ((original-char (char-after (point))))
    (if (null original-char)
	;; if not on a character, start at middle
	(setq original-char ?m)
      (message "got char %c" original-char)
      (delete-char 1))
    (let* ((pos (point))
	   (contents (minibuffer-completion-contents))
	   (fred (message "contents %S" contents))
	   (possibilities (possibilities-above original-char contents)))
      (message "possibilities %S" possibilities)
      (if possibilities
	  (progn
	    (delete-minibuffer-contents)
	    (insert (car possibilities)))
	(message "No further completions")
	(insert original-char))
      (goto-char pos))))

(defun joystick-valid-char-decrement ()
  "Decrement the character at point, using completion."
  (interactive)
  (unless (minibufferp)
    (error "Command joystick-valid-char-decrement only valid in minibuffer"))
  (let* ((original-char (char-after (point))))
    (if (null original-char)
	;; if not on a character, start at the top and work down
	(setq original-char ?z)
      (message "got char %c" original-char)
      (delete-char 1))
    (let* ((pos (point))
	   (contents (minibuffer-completion-contents))
	   (fred (message "contents %S" contents))
	   (possibilities (possibilities-below original-char contents)))
      (message "possibilities %S" possibilities)
      (let ((char-below (if (and (stringp (car possibilities))
				 (not (zerop (length (car possibilities)))))
			    (aref (car possibilities) 0)
			  ?m)))
	(while (= (and (stringp (cadr possibilities))
		       (> (length (cadr possibilities)) 0)
		       (aref (cadr possibilities) 0))
		  char-below)
	  (setq possibilities (cdr possibilities))))
      (if possibilities
	  (progn
	    (delete-minibuffer-contents)
	    (insert (car possibilities)))
	(message "No further completions")
	(insert original-char))
      (goto-char pos))))

(defun joystick-char-right-or-new ()
  "Move to the right if possible, otherwise add \"m\" at end of buffer.
\"m\" is chosen for being near the middle of the alphabet, so
should minimize the average number of bumps to get to any
particular letter."
  (interactive)
  (if (eobp)
      (progn
	(insert "m")
	(backward-char 1))
    (forward-char 1)))

(defun joystick-completing-char-right-or-new ()
  "Move to the next choice point if possible, otherwise add \"m\" at end.
\"m\" is chosen for being near the middle of the alphabet, so
\(depending on the distribution of letters in the completions)
should minimize the average number of bumps to get to any
particular letter."
  (interactive)
  (let ((contents (minibuffer-completion-contents)))
    (message "contents %S" contents)
    (if (eobp)
	(let* ((pos (point))
	       (starting-char ?m)
	       (possibilities (joystick-minibuffer-possibilities
			       (format "^%s[-%c-%c]"
				       contents
				       starting-char
				       (end-of-class starting-char)))))
	  (message "was at end")
	  (insert (car possibilities))
	  (goto-char pos))
      (let ((common (try-completion (if (string= contents "")
					"m"
				      contents)
				    minibuffer-completion-table
				    minibuffer-completion-predicate)))
	(message "was not at end")
	(delete-minibuffer-contents)
	(insert common)))))

(defun joystick-gamepad-minibuf-setup ()
  "Set up gamepad-style joystick bindings for using the minibuffer."
  (interactive)

  ;; (global-set-key [ PiBt-ThumbBtn2-up ] 'find-file)
)

(provide 'joystick-minibuf)

;;; joystick-minibuf.el ends here
