;;;; versor-demo.el -- demo for versor and languide
;;; Time-stamp: <2006-05-04 19:03:51 john>

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

(require 'versor)
(provide 'versor-demo)

(defun versor-find-demo-files ()
  (let ((dirs load-path))
    (catch 'found
      (while dirs
	(if (file-exists-p (expand-file-name "versor.el" (car dirs)))
	    (throw 'found
		   (expand-file-name "demo"
				     (file-name-directory (car dirs))))
	  (setq dirs (cdr dirs))))
      nil)))

(defun demo-find-file (file)
  "Find file. Get rid of an old buffer visiting it first, to prevent interaction about it."
  (let ((buf (find-buffer-visiting file))
	(find-file-hooks nil))
    (when buf
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    (find-file file)
    (font-lock-mode 1)
    (goto-char (point-min))))

(defvar demo-slowdown 1
  "*Multiplier for delays in running demo scripts.")

(defun demo-insert (string)
  "Insert STRING slowly"
  (let ((n (length string))
	(i 0))
    (while (< i n)
      (message "Typing %c" (aref string i))
      (insert (aref string i))
      (setq i (1+ i))
      (sit-for (* .3 demo-slowdown)))))

(defun run-demo-script (script)
  "Run demo SCRIPT."
  ;; (message "Running script %S" script)
  (save-window-excursion
    (delete-other-windows)
    (let ((steps script)
	  (temp-buffer-setup-hook nil)
	  (versor-display-highlighted-choice-time demo-slowdown))
      (while steps
	(let ((step (car steps)))
	  ;; (message "Step %S" step) (sit-for 1)
	  (cond
	   ((consp step)
	    (if (integerp (car step))
		(let ((count (car step))
		      (sub-script (cdr step)))
		  ;; (message "Repeating %S %d times" sub-script count)
		  (while (> count 0)
		    ;; (message "-- recursing to run %S" sub-script)
		    (run-demo-script sub-script)
		    (setq count (1- count))))
	      (when (commandp (car step))
		(message (substitute-command-keys
			  (format "Typing \\[%s]" (symbol-name (car step)))))
		(sit-for demo-slowdown))
	      (delete-other-windows)
	      (eval step)
	      (sit-for 0)))
	   ((stringp step)
	    (if (string-match "\n" step)
		(with-output-to-temp-buffer "*Demo commentary*"
		  (message nil)
		  (princ step))
	      (delete-other-windows)
	      (message step)))
	   ((null step)
	    (delete-other-windows)
	    (message nil))
	   ((numberp step)
	    (sit-for (* step demo-slowdown)))))
	(setq steps (cdr steps))))))

(defun versor-lisp-demo-1 ()
  "Helper function for versor-demo."
  (interactive)
  (run-demo-script
   '("Welcome to the versor demo.

This is an animated demonstration, with commentary, of the main versor commands.
We begin with editing an emacs-lisp file.

Watch the echo area (minibuffer) for commentary on what is going on."
     5
     "By default, versor starts in cartesian co-ordinates, with the cursor keys moving
by characters and lines." 2
     (versor-select-named-meta-level "cartesian")
     (versor-select-named-level "chars")
     (7 (versor-over-next) .5)
     "Note that versor tells you what kind of piece of code you have selected.

Some of the more advanced commands use this information, for to decide
whether the current selection is suitable to have a conditional
statement wrapped around it.

You can turn this feature off, if it is slowing your system, or annoying you." 2
     (2 (versor-over-prev) .5)
     (12 (versor-next) .5)
     "Using the M- modifier of the cursor keys, you can change the co-ordinate system.

Here, we step to the \"structural\" co-ordinates, selecting \"expressions\"
and \"depth\" as our pair of co-ordinates." 4
     (versor-demo-step-to-meta-level "structural") 1
     (versor-demo-step-to-level "exprs") 1
     "We can move by s-expressions" 2
     (5 (versor-next) 1)
     "And can also move by depth.

Note that two highlights appear after each depth move.

Doing DEL at such a time would put the two highlighted areas into adjacent
items on the kill-ring." 8
     (4 (versor-over-next) 1)
     (2 (versor-over-prev) 1)
     (3 (versor-next) 1)
     (3 (versor-over-next) 1)
     (versor-next)
     "Pressing DEL will delete the current selection, and tidy up whitespace
around it. If the selection is a multi-part one, the parts will go onto successive
elements of the kill-ring."
     2
     (versor-kill) 2
     "Typing ordinary text, and non-versor commands, still works as usual:" 1
     (demo-insert " (concat \"^\" ")
     (yank) 1
     (demo-insert ")") 3
     )))

(defun versor-c-demo-1 ()
  "Helper function for versor-demo."
  (interactive)
  (run-demo-script
   '("Now we look at the statement-based co-ordinates, using C as an example.

The co-ordinates used are called \"statements\" and \"statement-parts\".

Their definitions are language-specific. This release of versor supports
the Lisp and C families of languages, but more are planned."
     4
     (versor-demo-step-to-meta-level "program")
     (versor-demo-step-to-level "statement-parts")
     "First, we step down a couple of whole statements at the top level;
these are function definitions."
     2
     (2 (versor-over-next) 1)
     "Then we select the head of the function..." 
     1
     (versor-next) 1
     "And now the body of it"
     2
     (versor-next) 1
     "If we now go down some more statements, we will be moving inside the function."
     2
     (4 (versor-over-next) 1)
     "Now we can select the head or body of the statement."
     2
     (versor-next) 1 (versor-next) 1
     "When in the statement body, we can step among its constituent statements." 2
     (versor-over-next) 1
     (versor-over-next) 1
     "As well as selecting head, body (or tail), we can also select the framework
of a statement, that is, the syntax around the variable parts of it.

If we deleted that, it would put several strings onto the kill-ring." 2
     (versor-prev) 2
     "We can also select the statement containing the current selection.
This is a way of going back up the syntax of the code being edited." 2
     (versor-prev) 2
     ;; (message "selection is %S" (versor-get-current-item)) 2
     "Next, we see some high-level editing operations, starting with turning
a block of code into a separate function and substituting a call to it.

Support for these is provided by versor's companion package, languide.
You can also use such languide commands directly (without versor),
applying them to the GNUemacs region." 8
;; (message "selection is %S" (versor-get-current-item)) 2 
     (versor-languide-convert-selection-to-function "get_graphic") 4
     (beginning-of-defun)
     (versor-over-prev)
     (versor-over-prev) 2
     "The orange highlighting draws attention to complex automatic changes,
some of which may be some distance from the point at which you issued the command." 4
     (languide-remove-auto-edit-overlays) 2
     "Now we make some code conditional.
We select two statements here, by extending the selection." 4
     (versor-next) 1
     (versor-next) 1
     (versor-over-next) 1		; step into the function body
     (versor-next) 1
     (versor-next) 1
     "We can extend the selection to include the next statement,
but first we have to make \"statements\" be the current level,
because extending the selection works only at the current level,
and not its parent level." 6
     (versor-demo-step-to-level "statements") 1
     (versor-next) 1
     (versor-extend-item-forwards) (versor-extend-item-forwards) 1
     "Extended the selection" 2
     "Now we will make the selected code conditional.

If the selection had been exactly the body of an existing if-then, the new
condition would have been added as a further term to the condition of the
existing if-then statement." 4
     (versor-languide-make-conditional "disallow_null") 4
     (languide-remove-auto-edit-overlays) 2)))

(defun versor-lisp-demo-2 ()
  "Helper function for versor-demo."
  (interactive)
  (goto-char (point-min))
  (run-demo-script
   '("Now we return to the emacs-lisp demonstration buffer to show the
automatic conversion of an expression to a variable.

This works in C as well, in which it tries to deduce the appropriate
type of the variable, too. We demonstrate it in Lisp to show that this
level of operation is cross-language."
     10
     (search-forward "languide-region-type")
     (versor-select-named-meta-level "structural")
     (versor-select-named-level "exprs")
     (4 (versor-next) .2)
     (versor-over-next) .2
     (versor-next) .2 (versor-over-next) .2
     (versor-next) .2 (versor-over-next) .2
     (versor-next) .2 (versor-next) .2
     (versor-over-next)
     (3 (versor-next) .2)
     (versor-over-next) .2
     (2 (versor-next) .2) (versor-prev) (versor-prev) 
     "Having selected the expression, we give a single command to convert it
to a variable at a suitable scope." 2
     (versor-languide-convert-selection-to-variable "is-digit")
     "Note that the command has determined the widest scope at which the new
variable can be defined -- all the variables needed for the expression
are in scope at that point." 10
     )))

(defun versor-demo ()
  "Demonstrate versor."
  (interactive)
  (unless (memq 'versor-current-level-name global-mode-string)
    (versor-setup 'arrows 'arrows-misc 'text-in-code 'verbose))
  (save-window-excursion
    (let* ((demo-dir (versor-find-demo-files))
	   (lisp-buffer nil))
      (unless (file-directory-p demo-dir)
	(setq demo-dir (read-file-name "Could not find demo dir, please choose: ")))
      (mapcar (lambda (extension)
		(copy-file (expand-file-name (format "demo-text-orig.%s" extension)
					     demo-dir)
			   (expand-file-name (format "demo-text.%s" extension)
					     demo-dir)
			   t))
	      '("el" "c"))
      (let ((emacs-lisp-mode-hook nil))
	(demo-find-file (expand-file-name "demo-text.el" demo-dir)))
      (setq lisp-buffer (current-buffer))
      (versor-lisp-demo-1)

      (demo-find-file (expand-file-name "demo-text.c" demo-dir))
      (versor-c-demo-1)

      (let ((emacs-lisp-mode-hook nil))
	(demo-find-file (expand-file-name "demo-text.el" demo-dir)))
      (versor-lisp-demo-2)

      (run-demo-script
       '("This is the end of the versor demo." 10)))))

(defun versor-demo-step-to-level (level)
  (let* ((targets (versor-find-level-by-single-name level))
	 (target (cdr targets))
	 (stepper (if (> target versor-level)
		      'versor-out
		    'versor-in)))
    (unless (= versor-meta-level (car targets))
      (setq versor-meta-level (car targets))
      (versor-trim-meta-level)
      (versor-trim-level)
      (setq targets (versor-find-level-by-single-name level))
      (target (cdr targets))
      (stepper (if (> target versor-level)
		   'versor-out
		 'versor-in)))
    (while (not (= versor-level target))
      (call-interactively stepper)
      (sit-for demo-slowdown))))

(defun versor-demo-step-to-meta-level (meta-level)
  (let* ((target (versor-find-meta-level-by-name meta-level))
	 (stepper (if (> target versor-level)
			'versor-next-meta-level
		      'versor-prev-meta-level)))
    (while (not (= versor-meta-level target))
      (call-interactively stepper)
      (sit-for demo-slowdown))))

;;; end of versor-demo.el
