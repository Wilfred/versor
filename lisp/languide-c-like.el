;;;; languide-c-like.el -- C, java, perl definitions for language-guided editing
;;; Time-stamp: <2006-03-03 20:06:56 jcgs>
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

(require 'cl)
(provide 'languide-c-like)

(defun languide-c-back-to-possible-ender (bod)
  "Move point back to be at something that might end a C statement,
and is outside a comment or string, relative to BOD.
BOD is Beginning Of Defun, which is taken to be not in a comment or string."
  (let ((in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      (cond
       ((and nil (looking-at "[a-z_]"))	; inside a symbol -- go to start of symbol -- not sure why I wanted to do this -- anded out for now
	(skip-syntax-backward "w_")
	)
       (t
	(re-search-backward "[{;}]"
			    bod		; (point-min)
			    t)	      ; leaves point at start of match
	(languide:debug-message 'languide-c-back-to-possible-ender "Found a \"%s\" at %d" (match-string 0) (point))
	;; having found a character that can end a C statement,
	;; we now parse from the start of the defun up to the
	;; position of the character, to see whether the character
	;; is in code or in string-or-comment
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (languide:debug-message 'languide-c-back-to-possible-ender "parse-partial-sexp returned %S" result)
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (if (nth 3 result) t nil)	; only want a number in in-comment-or-string if it tells us a character position; this one gives us a character code
				      (nth 4 result)
				      (languide-c-inside-for-control)
				      ))
	  (languide:debug-message 'languide-c-back-to-possible-ender "in-comment-or-string=%S" in-comment-or-string)
	  (if in-comment-or-string
	      (if (numberp in-comment-or-string)
		  (goto-char in-comment-or-string)
		(backward-char 1)))))))))

(defun languide-c-inside-for-control ()
  "Return whether we seem to be in the head of a for loop.
Useful when we've got to a semicolon and need to know whether it terminates a statement.
If not at a semicolon (e.g. deeper inside an expression inside the for loop head) this
is liable to return the wrong result."
  (save-excursion
    (condition-case error-var
	(progn
	  (backward-up-list 1)
	  (backward-sexp 1)
	  (looking-at "for"))
      (error nil))))

(defun blank-between (a b)
  "Return whether there is only whitespace between A and B."
  (save-excursion
    (goto-char a)
    (= (- b a)
       (skip-chars-forward " \t\n\r" b))))

(defmodal move-into-previous-statement (c-mode perl-mode) ()
  "Move into the previous C or Perl statement.
Need only work if already at or just before the start of a statement."
  (interactive)
  (let ((done nil))
    (while (not done)
      (skip-chars-backward " \t\n")
      (cond
       ((save-excursion
	  (backward-char 2)
	  (looking-at "\\*/"))
	(search-backward "/*" (point-min) t))
       ((eq (char-after (1- (point))) ?})
	(backward-sexp 1)
	(setq done t))
       (t
	(backward-char 1)
	(setq done t))))))

(defmodal move-into-next-statement (c-mode perl-mode) ()
  "Move into the next C or Perl statement.
Need only work if already at or just beyond the end of a statement."
  (interactive)
  (skip-to-actual-code)
  (if (looking-at "{")
      (forward-sexp 1)
    (forward-char 1)))

(defun languide-previous-substatement ()
  "Go back a block, if at a block end, otherwise go back a statement."
;;;;;;;;;;;;;;;; check first what kind, do things conditionally!
  (goto-char (safe-scan-sexps (point) -1))
)

(defun continue-back-past-curly-ket (starting-point)
  "Continue to back to the start of a C statement, having got back to a closing curly bracket."
  ;; taken out-of-line for readability of surrounding code
  (let* ((close (point))
	 (following-code (skip-to-actual-code)))
    (languide:debug-message 'continue-back-past-curly-ket "From starting-point at %d, found close at %d, following code at %d is \"%s\"" starting-point close following-code (buffer-substring following-code (+ following-code 20)))
    (cond
     ;; first two cases are those where a keyword can follow a closing brace
     ;; and still be within a statement
     ((looking-at "\\<while\\>")
      (languide:debug-message 'continue-back-past-curly-ket "Got \"while\" at %d, looking back to see which kind" (point))
      ;; might be
      ;;   do { ... } * while ( ... )
      ;; or
      ;;   { ... } * while ( ... ) { ... } // with the first { ... } being a free-standing compound statement
      ;; and so must go back to check for the "do"
      ;; (languide-previous-substatement) ; we know it's a compound statement, or we wouldn't be in this function
      (goto-char (safe-scan-sexps (point) -2))
      (languide:debug-message 'continue-back-past-curly-ket "Going back to check for \"do\"  gets us to %d:\"%s\"" (point) (buffer-substring (point) (+ (point) 20)))
      (if (looking-at "\\<do\\>")
	  (languide:debug-message 'continue-back-past-curly-ket "at \"do\", so remaining there")
	(languide:debug-message 'continue-back-past-curly-ket "was not \"do\", going back to code after closing brace")
	;; go back to the "while"
	(goto-char following-code)))
     ((looking-at "\\<else\\>")
      ;; where we started was:
      ;;   if ( ... ) { ... } * else { ... }
      ;; and so we must go back over the "then", the condition, and the keyword
      (languide:debug-message 'continue-back-past-curly-ket "Got \"else\" following closing brace, so going back")
      ;; (languide-previous-substatement) ; we know it's a compound statement, or we wouldn't be in this function
      (goto-char (safe-scan-sexps (point) -3))
      (languide:debug-message 'continue-back-past-curly-ket "That got us to \"%s\" which I hope is an \"if\"" (buffer-substring (point) (+ (point) 20))))

     ;; having dealt with both the "{ ... } <keyword>" cases,
     ;; now see whether we started inside a statement that follows a closing brace
     ((>= starting-point following-code)
      ;; this case looks like:
      ;;   { ... } ab*cd // where the { ... } is a free-standing compound statement, or anything else
      ;; so we want to go to:
      ;;   { ... } *abcd
      (languide:debug-message 'continue-back-past-curly-ket "We started inside the statement following a closing brace, so go to the start of that statement")
      (goto-char following-code))

     ;; next, try going back a statement and a keyword
     ;; and seeing if that is an else
     ((save-excursion ; don't disturb things for the next clause's test
	;; we are looking for the cases like:
	;;   if (...) { ... } else { ... } *
	;; (languide-previous-substatement) ; we know it's a compound statement, or we wouldn't be in this function
	(goto-char (safe-scan-sexps close -2))
	;; let's see if that's got us to:
	;;   if (...) { ... } *else { ... }
	(looking-at "\\<else\\>"))
      (languide:debug-message 'continue-back-past-curly-ket "Got else")
      (goto-char (safe-scan-sexps close -2)) ; re-do the movement that we did inside the save-excursion above
      (languide-previous-substatement)
      (goto-char (safe-scan-sexps (point) -2))
      )

     ;; if it wasn't an else, try going back another sexp
     ((progn
	(goto-char (safe-scan-sexps close -3))
	(looking-at "\\<\\(if\\)\\|\\(while\\)\\|\\(for\\)\\|\\(until\\)\\>"))
      (languide:debug-message 'continue-back-past-curly-ket "Got if/while/for/until, staying there")
      )

     (t
      ;; Go back and see what was before the brace; could be
      ;;   if ( ... ) { ... }
      ;;   while ( ... ) { ... }
      ;;   until ( ... ) { ... }
      ;;   for ( ... ) { ... }
      ;; or it could just be a free-standing code block
      (languide:debug-message 'continue-back-past-curly-ket "Other case of closing brace, at \"%s\"" (buffer-substring (point) (+ (point) 20)))


      (languide:debug-message 'continue-back-past-curly-ket "Other case; going back to look before the brace... got \"%s\"" (buffer-substring (point) (+ (point) 20)))
      (languide:debug-message 'continue-back-past-curly-ket "Remaining there contentedly")
      (cond


       (t
	(languide:debug-message 'continue-back-past-curly-ket "Before the brace was not if/while/for/until/else; assuming plain block")
	(goto-char (safe-scan-sexps close -1))))))
    (languide:debug-message 'continue-back-past-curly-ket "finished classifying code at close")
    ))

(defvar debug-overlays nil)

(defun debug-overlay (from to text colour)
  (when nil 
    (let ((o (make-overlay from to)))
      (overlay-put o 'face (cons 'background-color colour))
      (overlay-put o 'before-string (format "[%s:" text))
      (overlay-put o 'after-string (format ":%s]" text))
      (if nil
	  (push o debug-overlays)
	(read-char)
	(delete-overlay o)))))

(defmodal beginning-of-statement-internal (c-mode perl-mode) ()
  "Move to the beginning of a C or Perl statement."
  (interactive)
  (mapcar 'delete-overlay debug-overlays)
  (setq debug-overlays nil)
  (let ((starting-point (point))
	;; get beginning of defun, so we can use parse-partial-sexp to
	;; see whether we have landed in a string or comment
	(bod (save-excursion
	       (beginning-of-defun 1)
	       (point))))
    (debug-overlay bod starting-point "back to bod" "red")
    (languide:debug-message 'beginning-of-statement-internal "")
    (languide:debug-message 'beginning-of-statement-internal "Starting beginning-of-statement-internal-c-perl at %d, with beginning-of-defun at %d" starting-point bod)

    (languide-c-back-to-possible-ender bod)

    (let ((possible-ender (point)))
      (debug-overlay possible-ender (1+ possible-ender) "possible-ender" "orange")
      (languide:debug-message 'beginning-of-statement-internal "now at possible ender %d: \"%s\", which is not in a comment or string" (point) (buffer-substring (point) (min (point-max) (+ (point) 20))))

      ;; We've now found a statement delimiter, and checked that it is a
      ;; real one, and not part of a string or comment. Now we might make
      ;; some adjustments, then finally move over any whitespace or comments
      ;; leading in to the actual statement.

      (cond
       ((looking-at "{")
	(debug-overlay (point) (1+ (point)) "found open as possible ender" "cyan")
	(languide:debug-message 'beginning-of-statement-internal "found open as possible ender")
	;; move one character forward (into the braced area) so that
	;; when we skip whitespace and comments, we will be at the
	;; start of the first statement in the braces
	(forward-char 1))

       ((looking-at "}")
	(debug-overlay (point) (1+ (point)) "found close as possible ender" "cyan")
	(languide:debug-message 'beginning-of-statement-internal "found close as possible ender at %d" (point))
	(forward-char 1)
	;; this will take us back past the rest of the statement
	;; todo: which is wrong in some cases, so fix that! Only go
	;; back if what is ahead of us is something that can follow a
	;; closing brace within a statement, so for example we should
	;; go back if at "do { ... }  * while (...)", but not if at
	;; "while (...) do { ... } *"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	(continue-back-past-curly-ket
	 starting-point	   ; is this the right place to start it from?
	 ))

       ((looking-at ";")
	(goto-char (match-end 0))
	(let ((following-code (skip-to-actual-code)))
	  (debug-overlay (point) (1+ (point)) "found semicolon as possible ender" "cyan")
	  (languide:debug-message 'beginning-of-statement-internal "found semicolon as possible ender at %d, code following it is at %d" (point) following-code)
	  ;; not sure what's going on here, document it!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  (if (<= following-code starting-point)
	      (progn
		;; this is the "abcd;    fg*hi" case
		(goto-char following-code)
		(debug-overlay (point) (1+ (point)) "code following semicolon is behind or at where we started, so here we are" "pale green")
		(languide:debug-message 'beginning-of-statement-internal "code following semicolon is behind or at where we started, so here we are")
		(if (looking-at "else")
		    (progn
		      ;; this is the "if (xyz) abcd;    el*se mnop;" case
		      (languide:debug-message 'beginning-of-statement-internal "Looking at else")
		      (goto-char (- possible-ender 1)) ; just into the "then" statement
		      (beginning-of-statement-internal)
		      ;;;;;;;;;;;;;;;; probably more to do here?
		      )))
	    (progn
	      ;; this is the "abcd;  *  fghi" case
	      (debug-overlay (point) (1+ (point)) "code following semicolon is after where we started, go back another statement" "pale green")
	      (languide:debug-message 'beginning-of-statement-internal "code following semicolon is after where we started, go back another statement")
	      (goto-char (- possible-ender 1))
	      (beginning-of-statement-internal))))
	(when (save-excursion ; this is old version, is it right????????????????
		(parse-partial-sexp (point) starting-point
				    nil	; targetdepth
				    t	; stopbefore
				    )
		(= (point) starting-point))
	  (debug-overlay (min starting-point (point)) (max starting-point (point))
			 "no code between it and where we were" "blue")
	  (languide:debug-message 'beginning-of-statement-internal
				  "no code between it and where we were (%d); am now at %d"
				  starting-point (point))
	  (backward-char 1)
	  ))))

    ;; now we're at a real statement delimiter
    (debug-overlay (point) (1+ (point)) "back to real delimiter" "yellow")
    (languide:debug-message 'beginning-of-statement-internal
			    "Now at real delimiter, skip to code; am at %d, starting-point is %d"
			    (point) starting-point)
    ;;;;;;;;;;;;;;;; why this "unless"? find out, and comment it!
    (if (blank-between (point) (1+ starting-point))
	(languide:debug-message 'beginning-of-statement-internal
				"in blank area at %d..%d" (point) (1+ starting-point))
      (languide:debug-message 'beginning-of-statement-internal
			      "final step of b-o-s-i: not in blank")
      (skip-to-actual-code starting-point))
      (debug-overlay (point) starting-point "Finished" "purple"))
  (languide:debug-message 'beginning-of-statement-internal
			  "beginning-of-statement-internal finished at %d" (point)))

(defmodal end-of-statement-internal (c-mode perl-mode java-mode) ()
  "Move to the end of a C, Perl or Java statement."
  (let ((old (point))
	;; get beginning of defun, so we can see whether we have landed in a
	;; string or comment
	(bod (save-excursion
	       (beginning-of-defun 1)
	       (point))))
    (languide:debug-message 'end-of-statement-internal "Starting end-of-statement-internal at %d, with beginning-of-defun at %d" old bod)
    ;; (if (looking-at "{") (backward-char 1))
    (let ((in-comment-or-string t))
      ;; keep looking for the possible start of a statement, and checking that
      ;; it is not part of a comment or string
      (while in-comment-or-string
	(re-search-forward "[{;}]" (point-max) t) ; leaves point at end of match
	(languide:debug-message 'end-of-statement-internal "Found a \"%s\" at %d" (match-string 0) (point))
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (languide:debug-message 'end-of-statement-internal "parse-partial-sexp returned %S" result)
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (languide-c-inside-for-control)
				      (if (nth 3 result) t nil)
				      (nth 4 result)
				      ))
	  (languide:debug-message 'end-of-statement-internal "in-comment-or-string=%S" in-comment-or-string)
	  (if in-comment-or-string
	      (forward-char 1)))))

    (languide:debug-message 'end-of-statement-internal "Now at %d=%c, which is not in a comment or string; the latest match, which is \"%s\", starts at %d"
			    (point) (char-after (point)) (match-string 0) (match-beginning 0))
    (cond
     ((= (char-after (1- (point))) ?{)
      (languide:debug-message 'end-of-statement-internal "Found block start")
      (backward-char 1)
      (forward-sexp 1)))
    (cond
     ((save-excursion
	(languide:debug-message 'end-of-statement-internal "skipping from %d to look for else" (point))
	(skip-to-actual-code)
	(languide:debug-message 'end-of-statement-internal "skipped to %d to look for else" (point))
	(looking-at "else"))
      (languide:debug-message 'end-of-statement-internal "Found ELSE")
      (forward-sexp 1)
      (end-of-statement-internal)
      )
     ((save-excursion
	(skip-to-actual-code)
	(looking-at "while"))
      ;; now look around to see whether this is part of "do {} while ()"
      ;; avert your eyes when reading this code, or re-write it for me!
      (when (save-excursion
	      (skip-to-actual-code)
	      (forward-sexp 2)
	      (if (looking-at "[ \t\r\n]")
		  (skip-to-actual-code))
	      (looking-at "[;}]"))
	(languide:debug-message 'end-of-statement-internal "That WHILE is part of a DO")
	(skip-to-actual-code)
	(forward-sexp 2)
	(if (looking-at "[ \t\r\n]")
	    (skip-to-actual-code))))))
  (languide:debug-message 'end-of-statement-internal
			  "end-of-statement-internal finished at %d" (point)))

(defvar c-binding-regexp-1
  "\\(\\(struct +\\)?[a-z][a-z0-9_]* *\\*?\\[?\\]?\\) *\\([a-z][a-z0-9_]*\\) *\\(=\\|;\\)"
  "A regexp for a common kind of C binding.")

(defmodal identify-statement (c-mode) (default)
  "Identify the current statement, or return DEFAULT.
We must be at the start of the statement already, otherwise
this does not have to work."
  (cond
   ((looking-at "\\(do\\)\\|\\(for\\)\\|\\(while\\)\\|\\(if\\)\\|\\(return\\)\\|\\(switch\\)\\|\\(continue\\)\\|\\(default\\)\\|\\(case\\)")
    (languide:debug-message 'identify-statement "identify-statement-c-mode found %s" (match-string 0))
    (let ((keyword-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (cond
       ((string= keyword-string "if")
	(save-excursion
	  (if (and (safe-forward-sexp 2)
		   (next-statement-internal 1)
		   (skip-to-actual-code)
		   (looking-at "else"))
	      'if-then-else
	    'if-then)))
       ((string= keyword-string "for") 'for)
       ((string= keyword-string "return") 'return)
       ((string= keyword-string "while") 'while-do)
       ((string= keyword-string "switch") 'switch)
       ((string= keyword-string "continue") 'continue)
       ((string= keyword-string "case") 'case)
       ((string= keyword-string "default") 'default)
       ((string= keyword-string "do") 'do-while)
       (t nil))))
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *[-+*/]?= *")))
    'assignment)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at "\\(++\\)\\|\\(--\\)")))
    'assignment)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *(")))
    'function-call)
   ;; todo: recognize variable declarations with and without initial values
   ((save-excursion
      (and (safe-forward-sexp 2)
	   (looking-at " *[=;] *")))
    ;; (message "vardef1")
    'variable-declaration)
   ((save-excursion
      (looking-at "[^;()=]+;"))
    ;; (message "vardef2")
    'variable-declaration)
   ((looking-at c-binding-regexp-1)
    ;; (message "vardef3")
    'variable-declaration)
   ((eq (char-after (point)) ?{)
    'progn)
   ((and (zerop (current-column))
	 (looking-at "\\(static\\|extern\\)? *\\(struct +\\)?\\*?\\([a-z][a-z0-9_]*\\)"))
    'defun)
   (t default)))

(defmodal insert-compound-statement-open (c-mode perl-mode) ()
  "Insert a block start."
  (insert "{ "))

(defmodal compound-statement-open (c-mode perl-mode) ()
  "Return a block start."
   "{")

(defmodal insert-compound-statement-close (c-mode perl-mode) ()
  "Insert a block end."
  (insert "}"))

(defmodal compound-statement-close (c-mode perl-mode) ()
  "Return a block end."
  "}")

(defmodal statement-container (c-mode perl-mode java-mode) ()
  "Select the container of the current statement."
  ;; needs to do the "not in string, not in comment" stuff, so we need 
  ;; the Beginning Of Defun to compare against
  (let* ((bod (save-excursion
		(beginning-of-defun 1)
		(point)))
	 (in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      ;; todo: this is wrong, it can find a preceding and closed container
      ;; (search-backward "{" (point-min) t) ; leaves point at end of match
      (backward-up-list)
      (let ((result (save-excursion (parse-partial-sexp bod (point)
							0 ; target-depth
							nil ; stop-before
							nil ; state
							nil ; stop-comment
							))))
	(languide:debug-message 'statement-container "parse-partial-sexp returned %S" result)
	(setq in-comment-or-string (or
				    ;; emacs19 doesn't give us that handy 8th element!
				    (nth 8 result)

				    ;;					(languide-c-inside-for-control)
				    ;;					(if (nth 3 result) t nil)
				    ;;					(nth 4 result)

				    ))
	(when in-comment-or-string (goto-char in-comment-or-string))
	(languide:debug-message 'statement-container "in-comment-or-string=%S" in-comment-or-string)
	;;	    (if in-comment-or-string
	;;		(forward-char 1))

	))
    ;; We are now at the opening brace (or have reached the outermost level, and were there anyway)
    (safe-scan-sexps (point) 1)))

(defun find-next-c-binding-outwards ()
  "Move to the next enclosing binding."
  (let ((binding-pattern "{"))
    (while (and (outward-once)
		(not (looking-at binding-pattern))))
    (looking-at binding-pattern)))

(defmodal variables-in-scope (c-mode) (whereat)
  "Return the list of variables in scope at WHEREAT."
  ;; todo: add parameters to the list (last)
  (save-excursion
    (goto-char whereat)
    (beginning-of-defun)
    (let ((bod (point))
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char whereat)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (find-next-c-binding-outwards)
	(save-excursion
	  (down-list)
	  (skip-to-actual-code)
	  ;; (message "Entering binding list got us to %d \"%s\")" (point) (buffer-substring-no-properties (point) (+ (point) 72)))
	  (while (looking-at c-binding-regexp-1)
	    ;; (message "Looking at %s as binding var %s, type %s" (match-string-no-properties 0) (match-string-no-properties 3) (match-string-no-properties 1))
	    (push (cons (match-string-no-properties 3) (match-string-no-properties 1)) variables)
	    (goto-char (match-end 0))
	    (skip-to-actual-code))))
      variables)))

(defmodal move-to-enclosing-scope-last-variable-definition (c-mode java-mode perl-mode) (&optional variables-needed)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.
Optional arguments list names of variables needed in the definition of the new one.
This lets clever implementations put the definition as far out as possible.")

(defmodal insert-variable-declaration (c-mode java-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  )

(defmodal insert-variable-declaration (perl-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  )

(defmodal insert-function-declaration (c-mode java-mode) (name result-type arglist body)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY.")

(defmodal insert-function-declaration (perl-mode) (name result-type arglist body)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY."
  (insert "sub " name "{\n"
	  "    my (")
  (let ((first t))
    (mapcar (lambda (arg)
	      (unless first (insert " ") (setq first nil))
	      (cond
	       ((stringp arg)
		(insert "$" arg))
	       ((symbolp arg)
		(insert "$" (symbol-name arg)))
	       ((consp arg)
		;; todo: perl-specific clever stuff here
		)
	       ))
	    arglist))
  (insert ") = @_;\n"
	  body
	  "}\n"))

(defmodal insert-function-call (c-mode java-mode perl-mode) (name arglist)
  "Insert a function call for a function called NAME taking ARGLIST"
  (insert name "(")
  (insert (mapconcat 'arg-name arglist ", "))
  (insert ")"))

(defmodal deduce-expression-type (c-mode java-mode perl-mode) (value-text)
  "Given VALUE-TEXT, try to deduce the type of it.")

(defmodal move-before-defun (c-mode java-mode perl-mode) ()
  "Move to before the current function definition."
  (c-mark-function))			; pollutes mark ring -- sorry

(defmodal block-statement-at-end-p (c-mode java-mode perl-mode) ()
  "Return whether we are at the end of a block statement."
  (skip-to-actual-code)
  (looking-at "}"))

(defstatement comment (c-mode)
  "Comment"
  (head "/\\* *")
  (body "/\\* *" (upto " *\\*/"))
  (tail " *\\*/")
  (create (template "/* " r " */")))

(defstatement comment (java-mode)
  "Comment"
  (head "// *")
  (body "/\\* *" (upto " *$"))
  (tail "$")
  (create (template "// " r n)))

(defstatement comment (perl-mode)
  "Comment"
  (head "# *")
  (body "# *" (upto " *$"))
  (tail "$")
  (create (template "# " r n)))

(defstatement comment (c-mode)
  "Comment"
  (head "/\\* *")
  (body "/\\* *" (upto " *\\*/"))
  (tail " *\\*/")
  (create (template "/* " r " */")))

(defstatement if-then (c-mode java-mode perl-mode)
  "If statement without else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (statement))
  (framework (remember "if") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "if (" p ") {" n>
		    r "}"))
  (begin-end "if () {" end "}")
  (begin-end-with-dummy "if (1) {" end "}"))

(defstatement if-then-else (c-mode java-mode perl-mode)
  "If statement with else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (statement))	; todo: make a new navigator that selects the simple statement, or the statements that make up a compound statement
  (tail "if" (expression) (statement) "else" (statement))
  (framework (remember "if") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code)
	     (if "{"
		 ((remember "{") (statements) (remember "}"))
	       (statement))
	     (remember "else")
	     (skip-to-actual-code)
	     (continue-if "{")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "if (" p ") {" n>
		    r "} else {"n>
		    p "}"))
  (begin-end "if () {" "} else {}")
  (begin-end-with-dummy "if (1) {" "} else {}"))

(defstatement while-do (c-mode java-mode perl-mode)
  "While statement."
  (head "while" (expression-contents))
  (body "while" (expression) (expression-contents))
  (create (template & > "while (" p ") {" n>
		    r "}"))
  (begin-end "while () {" "}")
  (begin-end-with-dummy "while (1) {" "}"))

(defstatement do-while (c-mode java-mode perl-mode)
  "Do-While statement."
  (head "do" (expression-contents) "while" (expression))
  (body "do" (expression-contents))
  (create (template & > "do {" r "} while (" p ")" n>)))

(defstatement for (c-mode java-mode perl-mode)
  "For statement."
  (head "for" (expression-contents))
  (body "for" (expression) (statement))
  (framework (remember "for") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "for (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement switch (c-mode java-mode)
  "Switch statement"
  (head "switch" (expression-contents))
  (body "switch" (expression) (statement))
  (framework (remember "switch") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "switch (" p ";" p ";" p ") {" n>
		    r "}")))


(defstatement defun (perl-mode)
  "Function definition"
  (head "sub" (expression-contents))
  (body "sub" (expression) (expression-contents))
  (create (template & > "sub " (p "Function name: ")
		    n>
		    "{" n>
		    r
		    "}" n))
  (begin-end "sub {" "}"))

(defstatement variable-declaration (perl-mode)
  "My variables"
  (head "my (" (expressions))
  (body "my" (expression) (expressions))
  (create (template & > "my (" p ")" n)))

(defstatement variable-declaration (c-mode java-mode)
  "Local variable"
  ;; todo: recognize local variables with and without initial values, as separate statement types
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (create (template & > " " p ";" n)))

(defstatement assignment (perl-mode c-mode java-mode)
  "Assignment"
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (create (template & > (p "Variable: ") " = " r ";")))

(defstatement function-call (perl-mode c-mode java-mode)
  "Function call"
  (head (expression))
  (body (expression) (expression-contents))
  (create (template & > (p "Function name: ") "(" r ")")))

(defstatement return (c-mode java-mode)
  "Return, with optional result"
  (head "return" (start-of-match) (from-start-of-statement))
  (body "return" (upto ";"))
  (create (template & > "return" r ";")))

(defstatement and (perl-mode c-mode java-mode)
  "And expression."
  (begin-end "( " " && )")
  (begin-end-with-dummy "( " " && 1)"))

(defstatement or (perl-mode c-mode java-mode)
  "Or expression."
  (begin-end "(" " || )")
  (begin-end-with-dummy "(" " || 0)"))

(defstatement and (perl-mode c-mode java-mode)
  "Not expression."
  (begin-end "(!" ")"))

;;; end of languide-c-like.el
