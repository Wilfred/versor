;;;; versor-voice.el
;;; Time-stamp: <03/10/31 16:20:28 jcgs>

(provide 'versor-voice)
(require 'versor)

(defvar vr-versor-dimension-command-list nil
  "Vocal commands for versor")

(defun define-versor-vocal ()
  "Define the vocal commands."
  (interactive)
  (dolist (level (versor:all-level-names))
    (let* ((name (intern (concat "versor:select-" level)))
	   (body `(lambda ()
		    ,(format "Set the versor dimension to %s." level)
		    (interactive)
		    (versor:select-named-level ,level)))
	   )
      ;; (message "Defining %S to be %S" name body)
      (fset name body)
      (pushnew (cons level name) vr-versor-dimension-command-list :test 'equal))))

(if (null vr-versor-dimension-command-list) (define-versor-vocal))

(defvar vr-versor-command-list
  '(
    ("next" . versor:next)
    ("onwards" . versor:next)
    ("back" . versor:prev)
    ("out" . versor:out)
    ("in" . versor:in)
    ("first" . versor:start)
    ("initial" . versor:start)
    ("last" . versor:end)
    ("final" . versor:end)
    ("over" . versor:out-briefly)
    ("depth" . versor:select-in/out)
    ("expressions" . versor:select-expr)
    ("copy this" . versor:copy)
    ("cut this" . versor:kill)
    ("mark this" . versor:mark)
    ("valof" . wander-yank)
    ("result is" . pick-up-sexp-at-point)
    ("return" . exit-recursive-edit)
    )
  "Individually defined versor voice commands (and friends).")

;;; end of versor-voice.el
