;;;; languide-c-like.el -- C, java, perl definitions for language-guided editing
;;; Time-stamp: <2004-05-21 14:34:06 john>
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

(defun skip-to-actual-code (&optional limit)
  "Skip forward, over any whitespace or comments, to the next actual code.
This assumes that we start in actual code too.
LIMIT, if given, limits the movement."
  (interactive)
  (parse-partial-sexp (point) (if limit limit (point-max))
		      0			; target-depth
		      t			; stop-before
		      nil		; state
		      nil		; stop-comment
		      )
  (point))

(defun languide-c-back-to-possible-ender (bod)
  "Move point back to be at something that might end a C statement,
and is outside a comment or string, relative to BOD.
BOD is Beginning Of Defun, which is taken to be not in a comment or string."
  (let ((in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      (cond
       ((and nil (looking-at "[a-z_]"))
	(skip-syntax-backward "w_")
	)
       (t
	(re-search-backward "[{;}]" (point-min) t) ; leaves point at start of match
	(message "Found a \"%s\" at %d" (match-string 0) (point))
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (message "parse-partial-sexp returned %S" result)
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (if (nth 3 result) t nil)	; only want a number in in-comment-or-string if it tells us a character position; this one gives us a character code
				      (nth 4 result)
				      (languide-c-inside-for-control)
				      ))
	  (message "in-comment-or-string=%S" in-comment-or-string)
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
      (backward-char 2)
      (if (looking-at "\\*/")
	  (search-backward "/*" (point-min) t)
	(forward-char 2)
	(setq done t))))
  (backward-char 1))

(defmodal move-into-next-statement (c-mode perl-mode) ()
  "Move into the next C or Perl statement.
Need only work if already at or just beyond the end of a statement."
  (interactive)
  (skip-to-actual-code)
  (forward-char 1))

(defun languide-previous-substatement ()
  "Go back a block, if at a block end, otherwise go back a statement."
;;;;;;;;;;;;;;;; check first what kind, do things conditionally!
  (goto-char (safe-scan-sexps (point) -1))
)

(defun continue-back-past-curly-ket (starting-point)
  "Continue to back to the start of a C statement, having got back to a closing curly bracket."
  ;; taken out-of-line for readability of surrounding code
  (interactive)				; for testing
  (let* ((close (point))
	 (following-code (skip-to-actual-code)))
    (message "From starting-point at %d, found close at %d, following code at %d is \"%s\"" starting-point close following-code (buffer-substring following-code (+ following-code 20)))
    (cond
     ;; first two cases are those where a keyword can follow a closing brace
     ;; and still be within a statement
     ((looking-at "\\<while\\>")
      (message "Got \"while\" at %d, looking back to see which kind" (point))
      ;; might be
      ;;   do { ... } * while ( ... )
      ;; or
      ;;   { ... } * while ( ... ) { ... }
      ;; and so must go back to check for the "do"
      (languide-previous-substatement)
      (goto-char (safe-scan-sexps (point) -1))
      (message "That get us to %d:\"%s\"" (point) (buffer-substring (point) (+ (point) 20)))
      (unless (looking-at "\\<do\\>")
	(message "was not \"do\"")
	;; go back to the "while"
	(goto-char following-code)))
     ((looking-at "\\<else\\>")
      ;;   if ( ... ) { ... } * else { ... }
      ;; and so must go back
      (message "Got \"else\" following closing brace, so going back")
      (languide-previous-substatement)
      (goto-char (safe-scan-sexps (point) -2))
      (message "That got us to \"%s\" which I hope is an \"if\"" (buffer-substring (point) (+ (point) 20))))

     ;; having dealt with both the "{ ... } <keyword>" cases,
     ;; now see whether we started inside a statement that follows a closing brace
     ((> starting-point following-code)
      (message "We started inside the statement following a closing brace, so go to the start of that statement")
      (goto-char following-code))

     ;; next, try going back a statement and a keyword
     ;; and seeing if that is an else
     ((progn
	(languide-previous-substatement)
	(goto-char (safe-scan-sexps close -1))
	(looking-at "\\<else\\>"))
      (message "Got else")
      (languide-previous-substatement)
      (goto-char (safe-scan-sexps (point) -2))
      )

     ;; if it wasn't an else, try going back another sexp
     ((progn
	(goto-char (safe-scan-sexps (point) -1))
	(looking-at "\\<\\(if\\)\\|\\(while\\)\\|\\(for\\)\\|\\(until\\)\\>"))
      (message "Got if/while/for/until")
      )

     (t
      ;; Go back and see what was before the brace; could be
      ;;   if ( ... ) { ... }
      ;;   while ( ... ) { ... }
      ;;   until ( ... ) { ... }
      ;;   for ( ... ) { ... }
      ;; or it could just be a free-standing code block
      (message "Other case of closing brace, at \"%s\"" (buffer-substring (point) (+ (point) 20)))


      (message "Other case; going back to look before the brace... got \"%s\"" (buffer-substring (point) (+ (point) 20)))
      (cond


       (t
	(message "Before the brace was not if/while/for/until/else; assuming plain block")
	(goto-char (safe-scan-sexps close -1))))))
    (message "finished classifying code at close")
    ))

(defmodal beginning-of-statement-internal (c-mode perl-mode) ()
  "Move to the beginning of a C or Perl statement."
  (interactive)
  (let ((starting-point (point))
	;; get beginning of defun, so we can use parse-partial-sexp to
	;; see whether we have landed in a string or comment
	(bod (save-excursion
	       (beginning-of-defun 1)
	       (point))))
    (message "")
    (message "Starting beginning-of-statement-internal-c-perl at %d, with beginning-of-defun at %d" starting-point bod)

    (languide-c-back-to-possible-ender bod)

    (let ((possible-ender (point)))

      (message "now at possible ender %d: \"%s\", which is not in a comment or string" (point) (buffer-substring (point) (min (point-max) (+ (point) 20))))

      ;; We've now found a statement delimiter, and checked that it is a
      ;; real one, and not part of a string or comment. Now we might make
      ;; some adjustments, then finally move over any whitespace or comments
      ;; leading in to the actual statement.

      (cond
       ((looking-at "{")
	(message "found open as possible ender")
	(forward-char 1))
       ((looking-at "}")
	(message "found close as possible ender at %d" (point)) (sit-for 2)
	(forward-char 1)
	(continue-back-past-curly-ket starting-point))

       ((looking-at ";")
	(let ((following-code (skip-to-actual-code)))
	  (message "found semicolon as possible ender at %d, code following it is at %d" (point) following-code)
	  (if (<= following-code starting-point)
	      (progn
		(message "code following semicolon is behind or at where we started, so here we are")
		(goto-char following-code)
		(if (looking-at "else")
		    (progn
		      (message "Looking at else")
		      (goto-char (- possible-ender 1))

		      ;;;;;;;;;;;;;;;; probably more to do here?
		      )))
	    (progn
	      (message "code following semicolon is after where we started, go back another statement")
	      (goto-char (- possible-ender 1))
	      (beginning-of-statement-internal))))
	(when (save-excursion ; this is old version, is it right????????????????
		(parse-partial-sexp (point) starting-point nil t)
		(= (point) starting-point))
	  (message "no code between it and where we were (%d); am now at %d" starting-point (point))
	  (backward-char 1)
	  ))))

    ;; now we're at a real statement delimiter
    (message "Now at real delimiter, skip to code; am at %d, starting-point is %d" (point) starting-point)
    ;;;;;;;;;;;;;;;; why this "unless"? find out, and comment it!
    (if (blank-between (point) (1+ starting-point))
	(message "in blank area at %d..%d" (point) (1+ starting-point))
      (skip-to-actual-code starting-point))))

(defmodal end-of-statement-internal (c-mode perl-mode) ()
  "Move to the end of a C or Perl statement."
  (let ((old (point))
	;; get beginning of defun, so we can see whether we have landed in a
	;; string or comment
	(bod (save-excursion
	       (beginning-of-defun 1)
	       (point))))
    (message "Starting end-of-statement-internal at %d, with beginning-of-defun at %d" old bod)
    ;; (if (looking-at "{") (backward-char 1))
    (let ((in-comment-or-string t))
      ;; keep looking for the possible start of a statement, and checking that
      ;; it is not part of a comment or string
      (while in-comment-or-string
	(re-search-forward "[{;}]" (point-max) t) ; leaves point at end of match
	(message "Found a \"%s\" at %d" (match-string 0) (point))
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (message "parse-partial-sexp returned %S" result)
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (languide-c-inside-for-control)
				      (if (nth 3 result) t nil)
				      (nth 4 result)
				      ))
	  (message "in-comment-or-string=%S" in-comment-or-string)
	  (if in-comment-or-string
	      (forward-char 1)))))

    (message "Now at %d=%c, which is not in a comment or string; the latest match, which is \"%s\", starts at %d"
	     (point) (char-after (point)) (match-string 0) (match-beginning 0))
    (cond
     ((= (char-after (1- (point))) ?{)
      (message "Found block start")
      (backward-char 1)
      (forward-sexp 1)))
    (cond
     ((save-excursion
	(skip-to-actual-code)
	(looking-at "else"))
      (message "Found ELSE")
      (forward-sexp 2)	  ; NB assumes using { } -- should change this
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
	(message "That WHILE is part of a DO")
	(skip-to-actual-code)
	(forward-sexp 2)
	(if (looking-at "[ \t\r\n]")
	    (skip-to-actual-code)))))))

(defmodal identify-statement (c-mode) (default)
  "Identify the current statement, or return DEFAULT.
We must be at the start of the statement already, otherwise
this does not have to work."
  (cond
   ((looking-at "\\(do\\)\\|\\(for\\)\\|\\(while\\)\\|\\(if\\)\\|\\(switch\\)\\|\\(continue\\)\\|\\(default\\)\\|\\(case\\)")
    (message "identify-statement-c-mode found %s" (match-string 0))
    (let ((keyword-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (cond
       ((string= keyword-string "if")
	(save-excursion
	  (forward-sexp 3)
	  (skip-to-actual-code)
	  (if (looking-at "else")
	      'if-then-else
	    'if-then)))
       ((string= keyword-string "for") 'for)
       ((string= keyword-string "while") 'while)
       ((string= keyword-string "switch") 'switch)
       ((string= keyword-string "continue") 'continue)
       ((string= keyword-string "case") 'case)
       ((string= keyword-string "default") 'default)
       ((string= keyword-string "do") 'do)
       (t nil)
       )
      )
    )
   ((save-excursion
      (forward-sexp 1)
      (looking-at " *=[-+*/]? *"))
    'assignment)
   ((save-excursion
      (forward-sexp 1)
      (looking-at "\\(++\\)\\|\\(--\\)"))
    'assignment)
   ((save-excursion
      (forward-sexp 1)
      (looking-at " *("))
    'function-call)
   ((save-excursion
      (forward-sexp 2)
      (looking-at " *[=;] *"))
    'variable-declaration)
   ((save-excursion
      (looking-at "[^;()=]+;"))
    'variable-declaration)
   ((eq (char-after (point)) ?{)
    'progn)
   (t default)
   ))

(defmodal insert-compound-statement-open (c-mode perl-mode) ()
  "Insert a block start."
  (insert "{ "))

(defmodal insert-compound-statement-close (c-mode perl-mode) ()
  "Insert a block end."
  (insert "}"))

(defmodal statement-container (c-mode perl-mode java-mode) ()
  "Select the container of the current statement."
  ;; needs to do the "not in string, not in comment" stuff
  (let* ((bod (save-excursion
		(beginning-of-defun 1)
		(point)))
	 (in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      (search-backward "{" (point-min) t) ; leaves point at end of match
      (let ((result (save-excursion (parse-partial-sexp bod (point)
							0 ; target-depth
							nil ; stop-before
							nil ; state
							nil ; stop-comment
							))))
	(message "parse-partial-sexp returned %S" result)
	(setq in-comment-or-string (or
				    ;; emacs19 doesn't give us that handy 8th element!
				    (nth 8 result)

				    ;;					(languide-c-inside-for-control)
				    ;;					(if (nth 3 result) t nil)
				    ;;					(nth 4 result)

				    ))
	(message "in-comment-or-string=%S" in-comment-or-string)
	;;	    (if in-comment-or-string
	;;		(forward-char 1))

	)))  )

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
  (body "if" (expression) (expression-contents))
  (create (template & > "if (" p ") {" n>
		    r "}")))

(defstatement if-then-else (c-mode java-mode perl-mode)
  "If statement with else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (expression-contents))
  (tail "if" (expression) (expression) "else" (expression-contents))
  (create (template & > "if (" p ") {" n>
		    r "} else {"n>
		    p "}")))

(defstatement while-do (c-mode java-mode perl-mode)
  "While statement."
  (head "while" (expression-contents))
  (body "while" (expression) (expression-contents))
  (create (template & > "while (" p ") {" n>
		    r "}")))

(defstatement do-while (c-mode java-mode perl-mode)
  "Do-While statement."
  (head "do" (expression-contents) "while" (expression))
  (body "do" (expression-contents))
  (create (template & > "do {" r "} while (" p ")" n>)))

(defstatement for (c-mode java-mode perl-mode)
  "For statement."
  (head "for" (expression-contents))
  (body "for" (expression) (expression-contents))
  (create (template & > "while (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement defun (perl-mode)
  "Function definition"
  (head "sub" (expression-contents))
  (body "sub" (expression) (expression-contents))
  (create (template & > "sub " (p "Function name: ")
		    n>
		    "{" n>
		    r
		    "}" n)))

(defstatement variable-declaration (perl-mode)
  "My variables"
  (head "my (" (expressions))
  (body "my" (expression) (expressions))
  (create (template & > "my (" p ")" n)))

(defstatement assignment (perl-mode c-mode java-mode)
  "Assignment"
  (head "=" (start-of-match) (preceding-expression))
  (body "=" (expression))
  (create (template & > (p "Variable: ") " = " r ";")))

(defstatement function-call (perl-mode c-mode java-mode)
  "Function call"
  (head (expression))
  (body (expression) (expression-contents))
  (create (template & > (p "Function name: ") "(" r ")")))
