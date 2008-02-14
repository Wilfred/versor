;;; joystick.el --- Lisp part of joystick interface for Emacs

;; Copyright (C) 2007, 2008  John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
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
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This listens (and occasionally talks) to a companion process which
;; is implemented by joylisp.c.

;; It makes the joystick events appear like extra function keys.  A
;; basic set of bindings is included (search this file for "Bindings") .

;; A help function attempts to draw a typical gamepad and label it
;; with the bindings.  On a typical gamepad, this might be the button
;; labelled "10" or "BaseBtn4".

;; More detail:

;; The companion process can be started and stopped by
;; `joystick-start' and `joystick-stop'.  `joystick-start' has the
;; rudiments of multiple joystick support (it lets you select the
;; device, and give it a prefix for the stuff it sends back to Emacs)
;; but doesn't as yet follow through on that -- for example, there is
;; just a plain variable for the joystick process.  But I do intend to
;; do that sometime, unless someone else does it first.

;; The joystick communication process sends s-exps to Emacs, which
;; `eval's them in a process filter.  Mostly, they run a function
;; `jse' (standing for JoyStick Event), which sticks its argument (the
;; event) into the unprocessed event queue.  This makes them look a
;; bit like function keys, and fortunately Emacs isn't limited to just
;; things which are defined as function keys; any symbol in the event
;; queue gets used that way (like, for example, those invented as menu
;; entries) .  You define them in the same way as function keys.
;; There's a starter set at the end of this file, designed for use
;; with a "Gamepad"-style controller.

;; Buttons are sent as "-down" and "-up" events, but they also act as
;; modifiers to each other.  If a button has been used as a modifier
;; (i.e. another one was pressed while it was down), it sends a
;; "-release" event instead of "-up".  This way, you can use the same
;; buttons as modifiers and commands, according to whether or not they
;; have actually modified anything by the time they were released.

;; Modifier names are abbreviated versions of the button names
;; (uppercase letters, lowercase letters immediately following
;; uppercase letters, digits are all kept, the rest are dropped).  The
;; modifier names come before the button name, in an attempt to align
;; with the Emacs practice of putting "C-" and "M-C-" etc at the start
;; of key names.

;; For example, you can get event sequences like these:

;;       Trigger-down
;;       Trigger-up

;;       Trigger-down
;;       Tr-TopBtn-down
;;       Tr-TopBtn-up
;;       Trigger-release

;; Note that if you're using "-up" codes for chording, it makes a
;; difference which button you take your finger off last.  This makes
;; for a potentially extremely subtle chording keyboard!

;; Axes are sent as "-next" and "-previous" events, and the rate at
;; which they are sent depends on how far the joystick has been pushed
;; in that direction.  Modifiers also apply to them.

;; There are a few other s-exps that the joystick communication
;; process sends, but these are mostly for information and some are
;; used "internally" by this package.  For example, as it initializes,
;; the joystick communication process reports back what buttons and
;; axes it has; the Lisp code collects those up for use as completion
;; tables.

;; To see for yourself what it sends, run "joylisp" directly from a
;; shell, and watch its output as you press things.  Type "quit" at it
;; to make it go away.

;; I've observed that occasionally my joystick "disappears" from the
;; system, and comes back as "/dev/js1" instead of "/dev/js0".
;; There's some code at the top of `joystick-start' to try to handle
;; things like this.

;; You can also send commands to control the joystick communication
;; process, using `joystick-command'.  This lets you configure the
;; sensitivity of each channel, and various things like that.

;; See the comment at the top of joylisp.c for what the commands are.

;; Some of the Lisp commands in this package (`joystick-set-rate',
;; etc) are packaged versions of `joystick-command'.

;; Things which it might be good to do to this:

;; (1) Complete the multiple joystick support

;; (2) Allow the connection to the joystick communication process to
;;     work over the network, so people can put their joystick on their
;;     X-server rather than having to have it locally on the host on which
;;     their Emacs is running

;;; History:

;; John Sturdy was already interested in non-keyboard Emacs input,
;; particularly for use with the high-level editing package Versor
;; <http://emacs-versor.sourceforge.net/>.

;; Shae Erisson asked about using a joystick with Emacs, on EmacsWiki.

;; John Sturdy eventually got a joystick (gamepad) and wrote this.

;; The general idea (of receiving s-exps from the process, and sending
;; simple line commands to it) came from Barry Jaspan's vr.el, which
;; listens to a glue program using Dragon's voice input API, and the
;; code started off from fragments of that.  The C code grew from a
;; fragment of Vojtech Pavlik's jstest.c.

;; Initially written 2007-08-29.

;;; Code:

(defgroup joystick nil
  "Parameters for the joystick reader."
  :group 'hardware)

(defcustom joystick-default-device "/dev/js0"
  "The device to use as the joystick."
  :group 'joystick
  :type '(file :must-match t))

(defcustom joystick-log nil
  "*Whether to log the joystick events to the message buffer."
  :group 'joystick
  :type 'boolean)

(defcustom joystick-program "joylisp"
  "The program for interfacing with the joystick."
  :group 'joystick
  :type 'string)

(defcustom joystick-graphical-program "xjoylisp"
  "Graphical version of the program for interfacing with the joystick.
This version should draw a picture of the gamepad or joystick, and
label it actively with legends."
  :group 'joystick
  :type 'string)

(defcustom joystick-graphical (not (null window-system))
  "Whether to use the graphical version of the joystick interface."
  :group 'joystick
  :type 'boolean)

(defcustom joystick-geometry "200x100+800+0"
  "X geometry for the joystick diagram."
  :group 'joystick
  :type 'string)

(defcustom joystick-auto-raise nil
  "Whether to pop the joystick window up as apparently needed."
  :group 'joystick
  :type 'boolean)

;;;; Choosing a joystick

(defun joystick-list-joysticks ()
  "Create a list of all joystick devices."
  ;; you might have to change this for other systems; it works OK on
  ;; Fedora 7
  (mapcar (lambda (j)
	    (cons (file-name-nondirectory j)
		  j))
	  (directory-files "/dev" t "js[0-9]+")))

(defvar joystick-all-joystick-devices
  (joystick-list-joysticks)
  "All devices that appear to be joysticks.
As an alist, for completing.")

;;;; Joystick process handling

(defvar joystick-reading-string nil
  "Storage for partially-read commands from the joylisp subprocess.")

(defun joystick-output-filter (p s)
  "Act on output of joystick interface process P that has sent S."
  ;; originally based on vr-mode.el, but re-written since then
  (unless (stringp s)
    (message "Non-string %S given to joystick-output-filter" s))

  (setq joystick-reading-string (concat joystick-reading-string s))

  (while (> (length joystick-reading-string) 0)
    (let* ((parsed (read-from-string joystick-reading-string))
	   (joystick-expr (car parsed))
	   (idx (cdr parsed)))
      (setq joystick-reading-string (substring joystick-reading-string
					       (1+ idx)))
      (when joystick-log
	(message "from joystick: %S" joystick-expr))
      (if (and (consp joystick-expr)
	       (fboundp (car joystick-expr)))
	  (eval joystick-expr)
	(message "Undefined joystick command %S" joystick-expr)))))

(defun joystick-sentinel (p s)
  "Sentinel function for joystick process P, getting status S."
  (if (equal s "finished\n")
      (progn
	(setq joystick-processes (delete (rassq p joystick-processes)))
	(when (eq p joystick-latest-process)
	  (setq joystick-latest-process nil)))
    (error "Joystick process exited with status \"%s\"" s)))

(defvar joystick-processes nil
  "Alist of the joystick processes, from device name to process.")

(defvar joystick-latest-process nil
  "The most recently-created process communicating with a joystick.")

(defun joystick-find-working-joystick ()
  "Make sure that `joystick-default-device' is valid."
  (setq joystick-all-joystick-devices (joystick-list-joysticks))
  (let ((all joystick-all-joystick-devices))
    (while (and all
		(not (file-exists-p joystick-default-device)))
      (setq joystick-default-device (cdar all)
	    all (cdr all)))))

(defcustom joystick-start-hook nil
  "Functions to run on starting a joystick.
The device name is passed in as an argument."
  :group 'joystick
  :type 'hook)

(defcustom joystick-initialized-hook nil
  "Functions to run on completing the start of a joystick.
The device name is passed in as an argument.
This is the best place to bind the joystick axes and buttons,
because it can use `joystick-label-modified' which knows the
button name->number mapping, and can tell the graphical version
of the interface program what labels to display."
  :group 'joystick
  :type 'hook)

(defcustom joystick-bindings-hook nil
  "Functions to run after making the joystick bindings.
The joystick device name is passed in as an argument."
  :group 'joystick
  :type 'hook)

(defcustom joystick-stop-hook nil
  "Functions to run on stoping a joystick.
The joystick device name is passed in as an argument."
  :group 'joystick
  :type 'hook)

(defun joystick-start (&optional device event-identifier other-identifier)
  "Start the joystick process on optional DEVICE.
Optionally give the device an EVENT-IDENTIFIER and
OTHER-IDENTIFIER, in case you have several joystick-type
devices.
Returns the process started, which is also stored in `joystick-latest-process'."
  ;; todo: keep an alist of device names to processes, for better
  ;; handling of multiple joysticks -- this is in progress, not yet
  ;; done for the reporting back of key names

  ;; todo: make this work over network sockets?
  (interactive
   (progn
     ;; first, scan to see which joysticks exist, as they have been
     ;; known to disappear and re-appear under other names
     (joystick-find-working-joystick)
     (if current-prefix-arg
	 (list (completing-read "Joystick: "
				joystick-all-joystick-devices
				nil t)
	       (read-from-minibuffer "Joystick event function prefix: "
				     "jse '")
	       (read-from-minibuffer "Joystick non-event function prefix: "
				     "joystick-"))
       nil)))
  (or device
      (progn
	(joystick-find-working-joystick)
	(setq device joystick-default-device)))
  (unless device
    (throw 'no-joystick nil))
  ;; set the connection type to use a pipe, because otherwise commands
  ;; we send back to the joystick get buffered up until there are huge
  ;; quantities of them

  ;; todo: could someone make a socket not buffer them up, so that we
  ;; can make this work over a network, and then people can attach the
  ;; joystick to their X-server rather than the machine on which
  ;; they're running Emacs?
  (let ((process-connection-type nil))
    (setq joystick-reading-string ""	; get rid of old stuff
	  joystick-latest-process
	  (start-process "joylisp"	; name
			 nil		; buffer
			 ;; program
			 (if joystick-graphical
			     joystick-graphical-program
			   joystick-program)
			 ;; args
			 "--geometry" joystick-geometry
			 "--verbose" ; for debugging
			 (if joystick-auto-raise
			     "--autoraise"
			   "")
			 "--device" device
			 "--event" (or event-identifier "jse '")
			 "--name" (or other-identifier "joystick-"))
	  joystick-processes (cons (cons device
					 joystick-latest-process)
				   joystick-processes)))
  ;; I keep suspecting there's a timing hazard here (in Emacs process
  ;; setup generally: what if Emacs gets delayed a bit and the process
  ;; starts producing useful output between `start-process' and
  ;; `set-process-filter'?)  If really worried about it, I'd make
  ;; joylisp.c do nothing until it receives a "start" command, which
  ;; I'd send after I've got the process filter set up.  However, I've
  ;; not seen it give any trouble yet.
  (set-process-filter joystick-latest-process 'joystick-output-filter)
  (set-process-sentinel joystick-latest-process 'joystick-sentinel)
  (set-process-query-on-exit-flag joystick-latest-process nil)
  (process-put joystick-latest-process 'device device)
  (run-hook-with-args 'joystick-start-hook device)
  joystick-latest-process)

(defun joystick-read-active-device (prompt)
  "Using PROMPT, get the name of an active joystick from the user."
  (let ((default (caar joystick-processes)))
    (completing-read (if default
			 (format "%s (%s) "
				 prompt default)
		       prompt)
		     joystick-processes
		     nil
		     t
		     nil
		     nil
		     default)))

(defun joystick-stop (&optional device)
  "Stop the joystick process on DEVICE.
If DEVICE is not given, use `joystick-latest-process'."
  (interactive
   (list
    (joystick-read-active-device "Stop joystick process on: ")))
  (unless device
    (when (processp joystick-latest-process)
      (setq device (process-get joystick-latest-process 'device))))
  (run-hook-with-args 'joystick-stop-hook device)
  (when device
    (let ((process (cdr (assoc device joystick-processes))))
      (condition-case evar
	  (when (processp process)
	    (joystick-command device "quit")
	    (sit-for 1)
	    ;; just in case:
	    (kill-process process)
	    )
	(error nil))))
  (setq joystick-processes
	(delete (assoc device joystick-processes)
		joystick-processes)))

(defun joystick-command (device command)
  "To the joystick interface program for DEVICE, send COMMAND."
  ;; todo: use the hypothetical alist of joysticks mentioned in the
  ;; todo comment for joystick-start; this also applies to
  ;; joystick-set-rate, joystick-set-sensitivity, joystick-hat-speed,
  ;; joystick-rumble, joystick-shock and anything else like that.
  (interactive
   (list (joystick-read-active-device "Send command to joystick: ")
	 (read-from-minibuffer "Joystick command: ")))
  (let ((process (cdr (assoc device joystick-processes))))
    (if (processp process)
	;; todo: these don't seem to be getting through, unless they
	;; fill the buffer -- how can we flush them?  They work OK
	;; when it's a pipe, but not for a socket
	(process-send-string process command)
      (error "No joystick process"))))

;;;; Specific commands to the joystick

(defvar joystick-latest-speeds nil
  "The last known speed of each joystick.")

(defun joystick-set-rate (device rate)
  "Set the joystick rate for DEVICE to RATE ticks per second."
  (interactive
   (list (joystick-read-active-device "Set rate for joystick: ")
	 (read-number "Rate: ")))
  (joystick-command device (format "tickrate %f\n" rate))
  (setq joystick-latest-speed rate))

(defun joystick-get-speed (device)
  "Get the current speed of the joystick DEVICE."
  (when (null (assoc device joystick-latest-speeds))
    (joystick-command device "tickrate\n")
    (while (null (assoc device joystick-latest-speeds))
      ;; The joystick communication process should now send back a
      ;; joystick-current-tick-rate s-exp; wait for it to come
      ;; through.  It is executed as an expression, so we don't have
      ;; to actually do anything here.
      (sit-for 1))
    (assoc device joystick-latest-speeds)))

(defun joystick-current-tick-rate (device rate)
  "Set the latest known speed for joystick DEVICE to RATE.
This is a command sent back by the joystick communication
process, in response to a \"tickrate\" command with no argument."
  (let ((pair (assoc device joystick-latest-speeds)))
    (if pair
	(rplacd pair rate)
      (push (cons device rate) joystick-latest-speeds))))

(defun joystick-faster (device)
  "Make the joystick DEVICE go a bit faster."
  ;; todo: find a way of passing in which joystick it is
  (interactive (list "/dev/js0"))
  (message "Event %S" last-command-event)
  (joystick-set-rate device (* 1.25 (joystick-get-speed device))))

(defun joystick-slower (device)
  "Make the joystick DEVICE go a bit slower."
  ;; todo: find a way of passing in which joystick it is
  (interactive)
  (joystick-set-rate device (* 0.8 (joystick-get-speed))))

(defun joystick-set-acceleration (device acceleration)
  "Set the joystick DEVICE ACCELERATION."
  (interactive
   (list
    (joystick-read-active-device "Set acceleration for joystick: ")
    (string-to-number (read-from-minibuffer "Set acceleration to: "))))
  (joystick-command device (format "acceleration %f\n" acceleration)))

(defun joystick-set-sensitivity (device sensitivity &optional axis)
  "Set the joystick DEVICE SENSITIVITY, either for all, or for optional AXIS."
  (interactive
   (list
    (joystick-read-active-device "Set sensitivity for joystick: ")
    (joystick-read-axis-name "Axis: " t)))
  (if (and axis (not (string= axis "all")))
      (joystick-command (format "sensitivity %s %d\n" axis sensitivity))
    (joystick-command (format "sensitivity %d\n" sensitivity))))

(defun joystick-hat-speed (device hat-speed &optional axis)
  "Set the joystick DEVICE HAT-SPEED, for all hats, or for optional AXIS."
  (interactive
   (list
    (joystick-read-active-device "Set hat speed for joystick: ")
    (joystick-read-axis-name "Axis: " t)))
  (if (and axis (not (string= axis "all")))
      (joystick-command (format "sensitivity %s %d\n" axis hat-speed))
    (joystick-command (format "sensitivity %d\n" hat-speed))))

(defun joystick-rumble (device)
  "Make the joystick DEVICE rumble, if it supports it."
  (interactive
   (list
    (joystick-read-active-device "Rumble joystick: ")))
  (joystick-command "rumble\n"))

(defun joystick-shock ()
  "Make the joystick shock, if it supports it."
  (interactive
   (list
    (joystick-read-active-device "Shock joystick: ")))
  (joystick-command "shock\n"))

;; todo: the rest of the joylisp commands

;;;; Getting data back from the joystick, including setup

(defvar joystick-buttons nil
  "A-list of buttons names.
Filled in as the joystick communication process starts.")

(defvar joystick-button-array nil
  "Array of button names, in numerical order.
Filled in by `joystick-init-done' which is run when the joystick
has finished describing its buttons.")

(defvar joystick-axis-array nil
  "Array of axis names, in numerical order.
Filled in by `joystick-init-done' which is run when the joystick
has finished describing its axes.")

(defvar joystick-button-abbrevs nil
  "A-list of button name abbreviations.
Filled in as the joystick communication process starts.")

(defvar joystick-button-regexps nil
  "List of regexps matching joystick buttons.
Filled in as the joystick communication process starts.")

(defvar joystick-axes nil
  "A-list of axis names.
Filled in as the joystick communication process starts.")

(defvar joystick-double-headed-axes
  "A-list of axis names.
Filled in after the joystick communication process starts,
and contains - and + versions of each axis..")

(defvar joystick-axis-regexps nil
  "List of regexps matching joystick axes.
Filled in as the joystick communication process starts.")

(defun joystick-begin-init (device)
  "Accept that a joystick is starting to initialize.")

(defun joystick-declare-version (device n)
  "Accept that, for DEVICE, a declaration that the version is N.")

(defun joystick-declare-buttons (device n)
  "Accept that, for DEVICE, that the button count is N.")

(defun joystick-declare-axes (device n)
  "Accept that, for DEVICE, that the axis count is N.")

(defun joystick-declare-name (device n)
  "Accept that, for DEVICE, a declaration that the name is N.")

(defun joystick-declare-button (device n name abbrev-name)
  "Declare that for DEVICE, button N is called NAME and ABBREV-NAME.
This is a command sent back to Emacs from the joystick communication process."
  ;; todo: use the hypothetical alist of joysticks mentioned in the
  ;; todo comment for joystick-start
  (add-to-list 'joystick-buttons (cons name n))
  (add-to-list 'joystick-button-abbrevs (cons abbrev-name n))
  ;; Store a regexp for modifying the help diagram
  (push (cons (format "\\[ *%s *\\]" name)
	      (vector (intern (concat (symbol-name name) "-up"))))
	joystick-button-regexps))

(defun joystick-declare-axis (device n name)
  "Declare that for DEVICE, axis N is called NAME.
This is a command sent back to Emacs from the joystick communication process."
  ;; todo: use the hypothetical alist of joysticks mentioned in the
  ;; todo comment for joystick-start
  (push (cons name n)
	joystick-axes)
  ;; Store a couple of regexps for modifying the help diagram
  (let ((name-string (symbol-name name)))
    (push (cons (format "\\[ *%s-p *\\]" name-string)
		(vector (intern (concat (symbol-name name) "-previous"))))
	  joystick-axis-regexps)
    (push (cons (format "\\[ *%s-n *\\]" name-string)
		(vector (intern (concat (symbol-name name) "-next"))))
	  joystick-axis-regexps)))

(defun joystick-double-axes (axes)
  "Double all of AXES."
  (let ((double nil))
    (dolist (axis axes)
      (let ((base (symbol-name (car axis)))
	    (n (* 2 (cdr axis))))
	(push (cons (intern (concat base "-previous")) n) double)
	(push (cons (intern (concat base "-next")) (1+ n)) double)))
    (sort double
	  (lambda (a b)
	    (< (cdr a) (cdr b))))))
	
(defun joystick-init-done (device)
  "Accept that a joystick DEVICE has completed its initialization."
  (setq joystick-buttons (sort joystick-buttons
			       (lambda (a b)
				 (< (cdr a) (cdr b))))
	joystick-axes (sort joystick-axes
			    (lambda (a b)
			      (< (cdr a) (cdr b))))
	joystick-double-headed-axes (joystick-double-axes joystick-axes)
	joystick-button-array (apply 'vector
				     (mapcar 'car joystick-buttons))
	joystick-axis-array (apply 'vector
				   (mapcar 'car joystick-double-headed-axes))
	)
  (run-hook-with-args 'joystick-initialized-hook device))

(defun joystick-label (device modifiers keymap label)
  (message "Joystick %s has label %s when on %d:%s"
	   device label modifiers keymap))

(defun joystick-label-axis (device modifiers keymap axis label)
  (message "Joystick %s has axis %d labelled %s when on %d:%s"
	   device axis label modifiers keymap))

(defun joystick-label-button (device modifiers keymap button label)
  (message "Joystick %s has button %d labelled %s when on %d:%s"
	   device button label modifiers keymap))

(defun joystick-confirm-labels (device)
  "Confirm the current labelling for DEVICE."
  (interactive
   (list (joystick-read-active-device "Send command to joystick: ")))
  (joystick-command device "labels\n"))

(defun joystick-command-acknowledge (device command)
  "Handle acknowledgement from DEVICE of COMMAND."
  (message "Acknowledgement: %s: %S" device command))

;;;; Getting control names

(defun joystick-read-button-name (prompt &optional allow-all)
  "Read a button name, using PROMPT.
Optionally with ALLOW-ALL, allow a completion \"all\"."
  ;; todo: use the hypothetical alist of joysticks mentioned in the
  ;; todo comment for joystick-start
  (completing-read prompt
		   (if allow-all
		       (cons (cons "all" nil)
			     joystick-buttons)
		        joystick-buttons)
		   nil
		   t))

(defun joystick-read-axis-name (prompt &optional allow-all)
  "Read an axis name, using PROMPT.
Optionally with ALLOW-ALL, allow a completion \"all\"."
  ;; todo: use the hypothetical alist of joysticks mentioned in the
  ;; todo comment for joystick-start
  (completing-read prompt
		   (if allow-all
		       (cons (cons "all" nil)
			     joystick-axes)
		     joystick-axes)
		   nil
		   t))

;;;; Other-window based functions, to give the second analogue
;;;; joystick something useful to do

(defmacro in-other-window (window-offset &rest body-forms)
  "In the WINDOW-OFFSETth other window, execute BODY-FORMS.
This means with the current buffer being the buffer of the other window.
If there is no other window, make one and put the next buffer there."
  `(let ((original-window (selected-window)))
     ;; (message "original-window %S" original-window)
     (when (<= (count-windows) ,window-offset)
       (set-window-buffer (split-window) (other-buffer)))
     (let ((window-to-use original-window))
       (dotimes (i ,window-offset)
	 (setq window-to-use (next-window window-to-use)))
       (select-window window-to-use)
       (with-current-buffer (window-buffer
			     window-to-use)
	 ,@body-forms)
       (select-window original-window))))

(defvar other-window-base-commands
  '(forward-char backward-char next-line previous-line)
  "Commands to base other-window- commands on.")

(defun make-other-window-commands (&optional base-commands)
  "Make commands to do things in the other window.
Base them on BASE-COMMANDS if given, otherwise on
`other-window-base-commands'."
  (dolist (suffix '(("other-window" . 1)
		    ("other-other-window" . 2)))
    (dolist (base-command (or base-commands
			      other-window-base-commands))
      (let ((name (intern
		   (concat (symbol-name base-command)
			   "-"
			   (car suffix))))
	    (command `(lambda (n)
			,(format "Like (%s N) but in the %s."
				 (symbol-name base-command)
				 (car suffix))
			(interactive "p")
			(in-other-window ,(cdr suffix)
					 (,base-command n)))))
	(fset name command)))))

;;;; Completion-related actions useful in the minibuffer

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
  "Helper function for `joystick-valid-char-decrement'.
Argument ORIGINAL-CHAR is the character to look for possibilities below.
Argument CONTENTS is the contents of the minibuffer."
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
  "Helper function for `joystick-valid-char-increment'.
Argument ORIGINAL-CHAR is the chracter to look for characters above.
Argument CONTENTS is the contents of the minibuffer."
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


;;;; Process the joystick events

(defvar jse-allow-all-events nil
  "Whether to allow all events, even unbound ones.")

(defvar jse-always-list nil
  "Whether to make joystick events always be lists.
Otherwise, they are lists only when there are args to give.")

(defvar jse-latest-keymap nil
  "The latest keymap for checking buttons in.
This is used to skip passing events which are unbound, while
still following prefix trees.")

(defun jse (event-type &rest event-args)
  "Process a joystick event, with EVENT-TYPE and perhaps some EVENT-ARGS."
  (unless joystick-reading-chords
    (unless jse-latest-keymap
      (setq jse-latest-keymap (current-global-map)))
    (let* ((event-vector (vector event-type))
	   (binding (lookup-key jse-latest-keymap event-vector)))
      (if (or binding
	      jse-allow-all-events
	      (key-binding event-vector)
	      (and (boundp 'versor-insertion-kind-alist)
		   (assoc event-type versor-insertion-kind-alist)))
	  (setq unread-command-events
		(nreverse (cons
			   (if (or jse-always-list
				   event-args)
			       (cons
				event-type
				event-args)
			     event-type)
			   (nreverse unread-command-events)))
		jse-latest-keymap (if (keymapp binding)
				      binding
				    nil))
	(when joystick-log
	  (message "%S not bound" event-type))))
    (when joystick-indicate-next-event
      (message "Event %S %S" event-type event-args)
      (setq joystick-indicate-next-event nil))))

(defvar joystick-reading-chords nil
  "Whether the joystick is reading chords.
This is an alternative way of reading groups of button presses,
instead of the normal modifier-and-action system.  It's unlikely
to make sense to use the two at exactly the same time, so this
variable lets you switch between them.  When it is non-nil,
chord-typing processing is done instead of command processing,
for example for typing characters using Braille or GKOS key
chords.")

(defun joystick-chord (chord)
  "Handle a joystick chord represented by CHORD.
Chord outputs are generated after the action outputs,
whenever a chord is completely released."
  (interactive "NChord (as number): ")
  (when joystick-reading-chords
    ;; (message "Got chord %d" chord)

    ;; Fill in your own code here if you want to do something with
    ;; chord typing
    ))

(defun all-sticks-initialized (&rest args)
  "This is run from the joystick process.
It happens when all the joystick devices have been initialized.
It is only significant if you're using a controller made of
multiple joystick devices; even then, it's probaby not really
significant, and we don't as yet do anything with it.")

(defun joystick-do-nothing ()
  "Do nothing.
This is bound to chords of large numbers of buttons, so that if
you realize you've got a chord wrong, you can cancel it by
pressing all the shoulder buttons before you release anything."
  (interactive))

(defvar joystick-indicate-next-event nil
  "Whether to indicate the next joystick event.")

(defun joystick-indicate-next-event ()
  "Indicate the next joystick event."
  (interactive)
  (setq joystick-indicate-next-event t))

;;;; Help on button assignments

(defun joystick-diagram-replace-label (sub-match key-sequence
						 binding
						 as-modifier)
  "Replace SUB-MATCH with help for KEY-SEQUENCE to BINDING / AS-MODIFIER.

The help can be a property of the symbol -- the major mode name
is tried -- should be safe as these symbols are probably not
greatly used other than for joystick -- or 'joystick-help.
Otherwise the binding symbol name is used."
  (let* ((available-length (- (match-end sub-match)
			      (match-beginning sub-match)))
	 (symbol (aref key-sequence 0))
	 (new-label (or (get symbol major-mode)
			(get symbol 'joystick-help)
			(get binding 'joystick-help)
			(symbol-name binding)))
	 (new-length (if (stringp as-modifier)
			 (+ (length new-label)
			    1
			    (length as-modifier))
		       (length new-label)))
	 (trimmed-label (if (< new-length available-length)
			    (let* ((all-padding (- available-length
						   new-length))
				   (before-padding (/ all-padding 2)))
			      ;; centre the label; we don't do half-spaces
			      (if as-modifier
				  (concat (make-string before-padding
						       ?\ )
					  new-label
					  "/"
					  as-modifier
					  (make-string (- all-padding before-padding)
						       ?\ ))
				(concat (make-string before-padding
						     ?\ )
					new-label
					(make-string (- all-padding before-padding)
						     ?\ ))))
			  (if (and (stringp as-modifier)
				   (not (string= as-modifier "")))
			      (let* ((proportion-off-label (/ (float (length new-label))
							      new-length))
				     (all-trimming (- new-length available-length))
				     (off-label (floor (* proportion-off-label
							  all-trimming)))
				     (off-mod-label (- all-trimming off-label)))
				(message "total off: %S; proportion %S; off %S(%S): %S; off %S(%S): %S"
					 all-trimming proportion-off-label
					 new-label (length new-label) off-label
					 as-modifier (length as-modifier) off-mod-label)
				(concat (substring new-label 0 (- off-label))
					"/"
					(substring as-modifier 0 (- off-mod-label))))
			    (substring new-label 0 available-length)))))
    (replace-match trimmed-label t t nil sub-match)))

(defvar joystick-gamepad-diagram-string "
  /----------------------------\\           /---------------------------\\front
 / [PinkieBtn        ]          \\---------/       [BaseBtn2         ]   \\ of
 | [TopBtn2          ]                            [BaseBtn          ]    | pad
 +-----------------------------------------------------------------------+
 |   [  Hat0Y-p   ]                                       [ThumbBtn ]    | top
 |[Hat0X-p   ][Hat0X-n ] [BaseBtn3 ] [BaseBtn4]     [Trigger ]  [TopBtn ]| of
 |   [  Hat0Y-n]+---------------+         +----------------+[ThumbBtn2  ]| pad
 +--------------| [    Y-p    ] |---------| [    Z-p     ] |------------+
    /           |[X-p  ][X-n   ]|         |[Rz-p  ][Rz-n  ]|          \\
   /            | [    Y-n    ] |         | [    Z-n     ] |           \\
  /             +---------------+         +----------------+            \\
 /               /                                        \\              \\
/_______________/                                          \\______________\\
"
  "String holding a diagram of the joystick.

Key names in square brackets are replaced (along with the square
brackets) by the help string for that key.")

(defvar joystick-gamepad-explanation "
In the following table, the characters in between [ ] in the
first column indicate the shoulder buttons \(the buttons on the
surface of the controller facing away from you), using ASCII
characters made of lines.  The lower row represents the ones
nearer you as seen if you were to flatten the controller like a
tigerskin rug \(i.e. actually the upper row of buttons on the
controller).  Thus, a \"=\" means both buttons on one side, \"_\"
means just the button nearer you on that side, and \"-\" means
just the button further from you.

\(v) indicates that a facility is part of emacs-versor \(see
http://www.emacs-versor.sourceforge.net/) which was designed
partly with few-button interfaces like this in mind.

btns | Prefixes               | commands             | modifer
0 buttons --------------------+----------------------+---------------
\[  ] |                        |                      |
1 button ---------------------+----------------------+---------------
\[- ] |       PiBt-            | other-buffer         | buf select
\[_ ] | ToBt2-                 | other-window         | cmd hist
\[ -] |                 BaBt2- | mark                 | dimension(v)
\[ _] |            BaBt-       | surround(v)          | extend(v)
2 buttons --------------------+----------------------+---------------
\[= ] | ToBt2-PiBt             | one-window/del-window| words etc
\[ =] |            BaBt-BaBt2- | M-x/menu             | sexps
\[--] |       PiBt-     BaBt2- | icicles-find-file/buf| statements
\[__] | ToBt2-     BaBt-       | commands             | chars/lines
\[-_] |       PiBt-     BaBt2- | transpose(v)         | insert(v)
\[_-] | ToBt2-          BaBt2- | alter(v)             | alter(v)
3 buttons --------------------+----------------------+---------------
\[_=] | ToBt2-     BaBt-BaBt2- |                      |
\[-=] |       PiBt-BaBt-BaBt2- |                      |
\[=_] | ToBt2-PiBt-BaBt-       |                      |
\[=-] | ToBt2-PiBt-     BaBt2- |                      |
4 buttons --------------------+----------------------+---------------
\[==] | ToBt2-PiBt-BaBt-BaBt2- | nothing              | nothing

One-button right-hand modifiers bring up lists to step through
with the hat switch; one-button left-hand modifiers are used by
versor.

All two-button modifier combinations along one side of the
quadrilateral control the units used by the hat switch and
copy/kill buttons.

Two-button modifiers along a diagonal do things that might change
text, etc.
"
"String explaining the layout of the gamepad bindings.")

(defun joystick-help ()
  "Display a diagram of the joystick button assignments.

If run from a joystick event (as it normally will be), work
out what modifiers were used to run this command, and apply
them to the button names to look up, thus generating a diagram
for the joystick in the current modifier state.

If setting up your own joystick bindings, to get a button
labelled with something short enough to fit, give the command
symbol a 'joystick-help property of the string you want.

To label a button as being a modifier, give the symbol naming the
-up event of that button a `joystick-modifier-help' property of
the string you want displayed for that modifier button."
  (interactive)
  (message "Last command event %S" last-command-event)
  (let* ((joystick-buffer "*Joystick*")
	 (event-string (if (symbolp last-command-event)
			   (symbol-name last-command-event)
			 ""))
	 (modifiers (if (string-match
			 "\\([A-Za-z0-9]+-\\)[A-Za-z0-9]+-\\(?:up\\|down\\|release\\)"
			 event-string)
			(match-string 1 event-string)
		      nil)))
    (save-window-excursion
      (save-excursion
	(condition-case evar
	    (progn
	      (with-output-to-temp-buffer joystick-buffer
                (princ joystick-gamepad-diagram-string)
		(princ joystick-gamepad-explanation))
	      (with-current-buffer joystick-buffer
		(let ((buffer-read-only nil))
		  (dolist (label-list (list joystick-button-regexps
					    joystick-axis-regexps))
		    (dolist (button label-list)
		      (goto-char (point-min))
		      (let* ((key-symbol (if modifiers
					     (intern
					      (concat modifiers
						      (symbol-name
						       (aref (cdr button) 0))))
					   (aref (cdr button) 0)))
			     (key-sequence (if modifiers
					       (vector key-symbol)
					     (cdr button)))
			     (binding (key-binding key-sequence))
			     (binding-as-modifier
			      (get key-symbol 'joystick-modifier-help)))
			(when (and (or binding binding-as-modifier)
				   (re-search-forward (car button) (point-max) t))
			  (joystick-diagram-replace-label
			   0 (cdr button)
			   binding binding-as-modifier)))))))
	      (fit-window-to-buffer (get-buffer-window joystick-buffer))
	      (read-event))
	  (error (message "help error: %S" evar) nil))))))

;;;; Bindings

(defun set-other-window-axes (modifiers
			      prefix
			      suffix
			      hook
			      &rest axes)
  "Set `other-window' axes with MODIFIERS to do PREFIX - SUFFIX, and apply HOOK, for AXES."
  (let ((axis-names '("Rz-previous" "Rz-next" "Z-previous" "Z-next")))
    (while (and axes axis-names)
      (let ((key (vector (intern (concat modifiers (car axis-names)))))
	    (function (intern (concat prefix (symbol-name (car axes)) suffix))))
	(message "Setting %S to do %S" key function)
	(when hook
	  (funcall hook function))
	(global-set-key key function))
      (setq axes (cdr axes)
	    axis-names (cdr axis-names)))))

(defun joystick-label-modified (modifiers label device)
  "Tell the joystick labelling system that MODIFIERS means LABEL on DEVICE."
  (when joystick-graphical
    (let* ((modifier-bits
	    (apply '+
		   (mapcar (lambda (name)
			     (let ((index (cdr
					   (assoc name
						  joystick-button-abbrevs))))
			       (if (integerp index)
				   (ash 1 index)
				 0)))
			   (split-string modifiers "-" t))))
	   (button-row (mapconcat
			(lambda (button)
			  (let* ((modified (concat modifiers
						   (if (or (equal modifiers "")
							   (string-match "-$" modifiers))
						       ""
						     "-")
						   (symbol-name button)
						   "-up"))
				 (fred (message "modified %S --> %S" button modified))
				 (binding (key-binding (vector (intern modified))))
				 (help (get binding 'joystick-help)))
			    (message "%s-->%s" modified binding)
			    (cond
			     (help help)
			     (binding (symbol-name binding))
			     (t ""))))
			joystick-button-array
			"|"))
	   (axis-row (mapconcat
		      (lambda (axis)
			(let* ((modified (concat modifiers
						 (symbol-name axis)))
			       (binding (key-binding (vector (intern modified))))
			       (help (get binding 'joystick-help)))
			  (message "%s-->%s" modified binding)
			  (cond
			   (help help)
			   (binding (symbol-name binding))
			   (t ""))))
		      joystick-axis-array
		      "|"))
	   (command (format "labels %d %s;%s;%s\n"
			    modifier-bits
			    label
			    axis-row
			    button-row)))
      (message "made command %S for device %S" command device)
      (joystick-command device command))))

(defun dired-non-mouse ()
  "Run `dired', on the default directory, not using the mousy version."
  (interactive)
  (let ((use-file-dialog nil))
    (dired default-directory)))

(defun find-this-tag ()
  "Find the tag at point, without prompting."
  (interactive)
  (find-tag (funcall (or find-tag-default-function
			 (get major-mode 'find-tag-default-function)
			 'find-tag-default))))

(defun find-this-function ()
  "Find the function at point, without prompting."
  (interactive)
  (find-function (function-called-at-point)))

(defun Info-follow-nearest-node-or-exit (&optional fork)
  "Like `Info-follow-nearest-node', but exits if not on a reference.
Optional FORK is as for `Info-follow-nearest-node'."
  (interactive)
  ;; todo: stop it prompting
  (condition-case evar
      (Info-follow-nearest-node fork)
    (error (Info-exit))))

(defun joystick-gamepad-setup (device)
  "Configure the gamepad buttons and axes.
DEVICE tells it which device this is for."
  ;; todo: find somewhere for imenu, and a nice way to drive it -- put it on the same button as "find", probably
  ;; todo: find somewhere for find-file; replicate it with icicles
  ;; todo: find somewhere for switch-to-buffer; replicate it with icicles
  ;; todo: find somewhere for save-all-buffers-no-ask
  ;; todo: arrange for the axes to be labelled, too
  (interactive)

  ;; -----+------------------------+----------------------+---------------
  ;; [  ] |                        |                      |
  ;; -----+------------------------+----------------------+---------------

  ;; character/line movements
  (global-set-key [ Hat0X-previous ] 'backward-char)
  (global-set-key [ Hat0X-next ] 'forward-char)
  (global-set-key [ Hat0Y-previous ] 'previous-line)
  (global-set-key [ Hat0Y-next ] 'next-line)

  (global-set-key [ X-previous ] 'backward-char)
  (global-set-key [ X-next ] 'forward-char)
  (global-set-key [ Y-previous ] 'previous-line)
  (global-set-key [ Y-next ] 'next-line)

  (make-other-window-commands)

  (set-other-window-axes "" "" "-other-window" nil
			 'backward-char
			 'forward-char
			 'previous-line
			 'next-line)
  (set-other-window-axes "BaBt6-" "" "-other-other-window" nil
			 'backward-char
			 'forward-char
			 'previous-line
			 'next-line)
  (set-other-window-axes "ToBt2-PiBt-" "" "-other-window" nil
			 'backward-word
			 'forward-word
			 'backward-sentence
			 'forward-sentence)
  (set-other-window-axes "ToBt2-PiBt-BaBt6-" "" "-other-other-window" nil
			 'backward-word
			 'forward-word
			 'backward-sentence
			 'forward-sentence)
  (set-other-window-axes "BaBt-BaBt2-" "" "-other-window" nil
			 'backward-sexp
			 'forward-sexp
			 'backward-up-list
			 'down-list)
  (set-other-window-axes "BaBt-BaBt2-BaBt6-" "" "-other-other-window" nil
			 'backward-sexp
			 'forward-sexp
			 'backward-up-list
			 'down-list)
  (set-other-window-axes "PiBt-BaBt2-" "" "-other-window" nil
			 'c-beginning-of-statement
			 'c-end-of-statement
			 'beginning-of-defun
			 'end-of-defun)
  (set-other-window-axes "PiBt-BaBt2-BaBt6-" "" "-other-other-window" nil
			 'c-beginning-of-statement
			 'c-end-of-statement
			 'beginning-of-defun
			 'end-of-defun)

  ;; These are now done by set-other-window-axes above:
  ;;   (global-set-key [ Rz-previous ] 'backward-char-other-window)
  ;;   (global-set-key [ Rz-next ] 'forward-char-other-window)
  ;;   (global-set-key [ Z-previous ] 'previous-line-other-window)
  ;;   (global-set-key [ Z-next ] 'next-line-other-window)

  ;;   (global-set-key [ BaBt6-Rz-previous ] 'backward-char-other-other-window)
  ;;   (global-set-key [ BaBt6-Rz-next ] 'forward-char-other-other-window)
  ;;   (global-set-key [ BaBt6-Z-previous ] 'previous-line-other-other-window)
  ;;   (global-set-key [ BaBt6-Z-next ] 'next-line-other-other-window)

  (put 'backward-char 'joystick-help "char--")
  (put 'forward-char 'joystick-help "char++")
  (put 'previous-line 'joystick-help "line--")
  (put 'next-line 'joystick-help "line++")

  ;; some simple editing

  ;;     /----------------------\           /----------------------\   front
  ;;    /  next-buffer/bufmenu   \---------/                        \   of
  ;;    |  other-window                                             |   pad
  ;;    +-----------------------------------------------------------+
  ;;    |       up                                        kill      |   top
  ;;    |  left    right   speed               help   copy    yank  |   of
  ;;    |      down  +-----------+         +-----------+  find      |   pad
  ;;    +------------|           |---------|           |------------+
  ;;       /         |   move    |	 |	     |	    \
  ;;      /	   |	       |	 |	     |	     \
  ;;     /	   +-----------+  	 +-----------+	      \
  ;;    /		    /				\ 	       \
  ;;   /_____________/				 \______________\

  ;; The buttons marked "save" and "yank" also do `exit-minibuffer' and
  ;; `minibuffer-complete', respectively, when in the minibuffer.

  (global-set-key [ Trigger-up ] 'kill-ring-save)
  (global-set-key [ ThumbBtn-up ] 'kill-region)
  (global-set-key [ TopBtn-up ] 'yank)
  (global-set-key [ ThumbBtn2-up ] 'find-this-tag)
  (define-key emacs-lisp-mode-map [ ThumbBtn2-up ] 'find-this-function)
  (define-key lisp-interaction-mode-map [ ThumbBtn2-up ] 'find-this-function)

  (put 'kill-ring-save 'joystick-help "copy")
  (put 'kill-region 'joystick-help "kill")
  (put 'yank 'joystick-help "yank")
  (put 'find-this-tag 'joystick-help "find")
  (put 'find-this-function 'joystick-help "find")

  (add-hook
   ;; I tried eval-after-load, but, to my puzzlement, Info-mode-map
   ;; wasn't defined when I expected it to be.  So I'm doing it this
   ;; way, to get the job done.
   'Info-mode-hook
   (lambda ()
     ;; We shouldn't be killing or yanking in Info buffers, so re-use
     ;; the buttons.  We take over the copy button as well, which could
     ;; have been used, but this gives us a nice trio of buttons, and
     ;; lets us establish a pattern of "next" and "previous" buttons
     ;; that we can use in mail readers etc too.
     (message "(boundp 'Info-mode-map) --> %S" (boundp 'Info-mode-map))
     (define-key Info-mode-map [ Trigger-up ] 'Info-prev)
     (define-key Info-mode-map [ ThumbBtn-up ] 'Info-up)
     (define-key Info-mode-map [ TopBtn-up ] 'Info-next)
     (define-key Info-mode-map [ ThumbBtn2-up ]
       'Info-follow-nearest-node-or-exit
       ;; Info-exit
       )
     (put 'Info-prev 'joystick-help "previous")
     (put 'Info-next 'joystick-help "next")
     (put 'Info-up 'joystick-help "up")
     (put 'Info-exit 'joystick-help "quit")))

  (add-hook
   'vm-mode-hook
   (lambda ()
     ;; much the same as for info-mode button setup
     (define-key vm-summary-mode-map [ Trigger-up ] 'vm-previous-message)
     (define-key vm-summary-mode-map [ ThumbBtn-up ] 'vm-delete-message)
     (define-key vm-summary-mode-map [ TopBtn-up ] 'vm-next-message)
     (define-key vm-mode-map [ Trigger-up ] 'vm-previous-message)
     (define-key vm-mode-map [ ThumbBtn-up ] 'vm-delete-message)
     (define-key vm-mode-map [ TopBtn-up ] 'vm-next-message)
     (put 'vm-previous-message 'joystick-help "previous")
     (put 'vm-delete-message 'joystick-help "delete")
     (put 'vm-next-message 'joystick-help "next")))

  (add-hook
   'rmail-mode-hook
   (lambda ()
     ;; much the same as for info-mode button setup
     ;; rmail message buffers
     (define-key rmail-mode-map
       [ Trigger-up ] 'rmail-previous-undeleted-message)
     (define-key rmail-mode-map [ ThumbBtn-up ] 'rmail-delete-forward)
     (define-key rmail-mode-map [ TopBtn-up ] 'rmail-next-undeleted-message)
     (put 'rmail-previous-undeleted-message 'joystick-help "previous")
     (put 'rmail-delete-forward 'joystick-help "delete")
     (put 'rmail-next-undeleted-message 'joystick-help "next")
     ;; rmail summary buffers
     (define-key rmail-summary-mode-map
       [ Trigger-up ] 'rmail-summary-previous-msg)
     (define-key rmail-summary-mode-map
       [ ThumbBtn-up ] 'rmail-summary-delete-forward)
     (define-key rmail-summary-mode-map
       [ TopBtn-up ] 'rmail-summary-next-msg)
     (put 'rmail-summary-previous-msg 'joystick-help "previous")
     (put 'rmail-summary-delete-forward' 'joystick-help "delete")
     (put 'rmail-summary-next-msg 'joystick-help "next")))

    ;; -----+------------------------+----------------------+---------------
    ;; [' ] |       PiBt-            | other-buffer         | buf select
    ;; -----+------------------------+----------------------+---------------

    ;; Note: the menu appears when you press PinkieBtn AND Hat-Downwards,
    ;; not immediately on PinkieBtn
  (global-set-key [ PiBt-Hat0Y-next ] 'electric-buffer-list)
  (eval-after-load "ebuff-menu"
    '(progn
       (define-key electric-buffer-menu-mode-map [ PiBt-Hat0Y-next ]
	 'next-line)
       (define-key electric-buffer-menu-mode-map [ PiBt-Hat0Y-previous ]
	 'previous-line)
       (define-key electric-buffer-menu-mode-map [ ToBt2-PinkieBtn-up ]
	 'Buffer-menu-mark)
       (define-key electric-buffer-menu-mode-map [ PiBt-TopBtn2-up ]
	 'Buffer-menu-mark)
       (define-key electric-buffer-menu-mode-map [ PiBt-Hat0X-next ]
	 'Buffer-menu-mark)
       (define-key electric-buffer-menu-mode-map [ PiBt-Hat0X-previous ]
	 'Buffer-menu-unmark)
       (define-key electric-buffer-menu-mode-map [ PiBt-ThumbBtn-up ]
	 'Buffer-menu-delete)
       (define-key electric-buffer-menu-mode-map [ PinkieBtn-release ]
	 'Electric-buffer-menu-select)))

  (global-set-key [ PiBt-Hat0X-next ] 'dired-non-mouse)
  (put 'dired-non-mouse 'joystick-help "dired")
  (eval-after-load "dired"
    '(progn
       (mapcar
	(lambda (prefix)
	  (define-key dired-mode-map
	    (vector (intern (concat prefix "Hat0X-next")))
	    'dired-next-line)
	  (define-key dired-mode-map
	    (vector (intern (concat prefix "Hat0X-previous")))
	    'dired-previous-line)
	  (define-key dired-mode-map
	    (vector (intern (concat prefix "Trigger-up")))
	    'dired-find-file-other-window)
	  (define-key dired-mode-map
	    (vector (intern (concat prefix "ThumbBtn-up")))
	    'dired-flag-file-deletion)
	  ;; subdirectories
	  (define-key dired-mode-map
	    (vector (intern (concat prefix "TopBtn-up")))
	    'dired-maybe-insert-subdir)
	  (define-key dired-mode-map
	    (vector (intern (concat prefix "Hat0Y-next")))
	    'dired-tree-down)
	  (define-key dired-mode-map
	    (vector (intern (concat prefix "Hat0Y-previous")))
	    'dired-tree-up))
	'("" "PiBt-"))
       ;; end of the mode
       (define-key dired-mode-map [ PinkieBtn-release ] 'dired-find-file)))

  ;; PinkieBtn by itself (without Hat) just goes to the next buffer;
  ;; the one beside it acts as a modifier to go the other way
  (global-set-key [ PinkieBtn-up ] 'next-buffer)
  (global-set-key [ BaBt2-PinkieBtn-up ] 'previous-buffer)
  (joystick-label-modified "PiBt" "Buffer/File" device)

  ;; -----+------------------------+----------------------+---------------
  ;; [. ] | ToBt2-                 | other-window         | cmd hist
  ;; -----+------------------------+----------------------+---------------

  (global-set-key [ TopBtn2-up ] 'other-window)
  (put 'other-window 'joystick-help "-->window")

  (global-set-key [ ToBt2-Hat0Y-next ] 'repeat-complex-command)
  (define-key minibuffer-local-map [ ToBt2-Hat0Y-next ] 'previous-history-element)
  (define-key minibuffer-local-map [ ToBt2-Hat0Y-previous ] 'next-history-element)
  (define-key minibuffer-local-map [ TopBtn2-release ] 'exit-minibuffer)
  (joystick-label-modified "ToBt2" "History" device)

  ;; -----+------------------------+----------------------+---------------
  ;; [ '] |                 BaBt2- | mark                 | dimension(v)
  ;; -----+------------------------+----------------------+---------------

  (global-set-key [ BaseBtn-up ] 'set-mark-command)
  (put 'set-mark-command 'joystick-help "mark")

  ;; -----+------------------------+----------------------+---------------
  ;; [ .] |            BaBt-       | surround(v)          | extend(v)
  ;; -----+------------------------+----------------------+---------------

  ;; these are set up in emacs-versor:versor-joystick.el

  ;; -----+------------------------+----------------------+---------------
  ;; [: ] | ToBt2-PiBt             | one-window/del-window| words etc
  ;; -----+------------------------+----------------------+---------------

  (global-set-key [ TopBtn2-up ] 'other-window)
  (global-set-key [ PiBt-TopBtn2-up ] 'delete-other-windows)
  (global-set-key [ ToBt2-PinkieBtn-up ] 'delete-window)

  (global-set-key [ ToBt2-PiBt-Hat0X-previous ] 'backward-word)
  (global-set-key [ ToBt2-PiBt-Hat0X-next ] 'forward-word)
  (global-set-key [ ToBt2-PiBt-Hat0Y-previous ] 'backward-sentence)
  (global-set-key [ ToBt2-PiBt-Hat0Y-next ] 'forward-sentence)
  (global-set-key [ ToBt2-PiBt-ThumbBtn2-up ] 'isearch-yank-word)
  (global-set-key [ ToBt2-PiBt-ThumbBtn-up ] 'kill-word)

  ;; dabbrev typically does a word, so put it in with words
  (global-set-key [ ToBt2-PiBt-TopBtn-up ] 'dabbrev-expand)

  (put 'backward-word 'joystick-help "<-word")
  (put 'forward-word 'joystick-help "word->")
  (put 'backward-sentence 'joystick-help "<-sent")
  (put 'forward-sentence 'joystick-help "sent->")
  (put 'dabbrev-expand 'joystick-help "dabbrev")
  (joystick-label-modified "ToBt2-PiBt-" "Words" device)

  ;; -----+------------------------+----------------------+---------------
  ;; [ :] |            BaBt-BaBt2- | M-x/menu             | sexps
  ;; -----+------------------------+----------------------+---------------

  ;; todo: make joystick-icicles match these
  (global-set-key [ BaBt-BaseBtn2-up ] 'execute-extended-command)
  (put 'execute-extended-command 'joystick-help "M-x")

  (let ((joystick-minibuf-autoload-help "Joystick minibuffer command.
Full documentation available when loaded."))
    (autoload 'joystick-char-right-or-new "joystick-minibuf.el"
      joystick-minibuf-autoload-help t)
    (autoload 'joystick-char-decrement "joystick-minibuf.el"
      joystick-minibuf-autoload-help t)
    (autoload 'joystick-char-increment "joystick-minibuf.el"
      joystick-minibuf-autoload-help t)
    (autoload 'joystick-completing-char-right-or-new "joystick-minibuf.el"
      joystick-minibuf-autoload-help t)
    (autoload 'joystick-valid-char-decrement "joystick-minibuf.el"
      joystick-minibuf-autoload-help t)
    (autoload 'joystick-valid-char-increment "joystick-minibuf.el"
      joystick-minibuf-autoload-help t))

  (dolist (this-map (list minibuffer-local-map
			  minibuffer-local-ns-map))
    (define-key this-map
      [ BaBt-BaBt2-Hat0X-next ] 'joystick-char-right-or-new)
    (define-key this-map
      [ BaBt-BaBt2-Hat0Y-previous ] 'joystick-char-decrement)
    (define-key this-map
      [ BaBt-BaBt2-Hat0Y-next ] 'joystick-char-increment))

  (dolist (this-map (list minibuffer-local-completion-map
			  minibuffer-local-must-match-map))
    (define-key this-map [ BaBt-BaBt2-Hat0X-next ]
      'joystick-completing-char-right-or-new)
    (define-key this-map [ BaBt-BaBt2-Hat0Y-previous ]
      'joystick-valid-char-decrement)
    (define-key this-map [ BaBt-BaBt2-Hat0Y-next ]
      'joystick-valid-char-increment))

  (dolist (this-map (list minibuffer-local-completion-map
			  minibuffer-local-must-match-map
			  minibuffer-local-map
			  minibuffer-local-ns-map))
    (define-key this-map [ BaBt-BaBt2-Hat0X-previous ] 'backward-char)
    (define-key this-map [ Trigger-up ] 'minibuffer-complete-and-exit)
    (define-key this-map [ ThumbBtn-up ] 'delete-char)
    (define-key this-map [ TopBtn-up ] 'minibuffer-complete-word)
    (define-key this-map [ ThumbBtn2-up ] 'abort-recursive-edit))

  (global-set-key [ BaBt2-BaseBtn-up ] 'joystick-menu)
  (autoload 'joystick-menu "joystick-menu"
    "Execute a command from the menubar system, using a joystick."
    t)
  (put 'joystick-menu 'joystick-help "menu")

  (global-set-key [ BaBt-BaBt2-Hat0X-previous ] 'backward-sexp)
  (global-set-key [ BaBt-BaBt2-Hat0X-next ] 'forward-sexp)
  (global-set-key [ BaBt-BaBt2-Hat0Y-previous ] 'backward-up-list)
  (global-set-key [ BaBt-BaBt2-Hat0Y-next ] 'down-list)
  (global-set-key [ BaBt-BaBt2-ThumbBtn-up ] 'kill-sexp)

  (put 'backward-sexp 'joystick-help "<-()-")
  (put 'forward-sexp 'joystick-help "-()->")
  (put 'backward-up-list 'joystick-help "<-(-)")
  (put 'down-list 'joystick-help "-(->)")
  (joystick-label-modified "BaBt-BaBt2-" "Sexps" device)

  ;; -----+------------------------+----------------------+---------------
  ;; [''] |       PiBt-     BaBt2- | icicles-find-file/buf| statements
  ;; -----+------------------------+----------------------+---------------

  ;; todo: icicles commands
  (global-set-key [ PiBt-BaBt2-Hat0X-previous ] 'c-beginning-of-statement)
  (global-set-key [ PiBt-BaBt2-Hat0X-next ] 'c-end-of-statement)
  (global-set-key [ PiBt-BaBt2-Hat0Y-previous ] 'beginning-of-defun)
  (global-set-key [ PiBt-BaBt2-Hat0Y-next ] 'end-of-defun)

  (put 'c-beginning-of-statement 'joystick-help "<--;")
  (put 'c-end-of-statement 'joystick-help "-->;")
  (put 'beginning-of-defun 'joystick-help "^(<--")
  (put 'end-of-defun 'joystick-help "-->)$")
  (joystick-label-modified "PiBt-BaBt2-" "Statements" device)

  ;; -----+------------------------+----------------------+---------------
  ;; [..] | ToBt2-     BaBt-       |                      | chars/lines
  ;; -----+------------------------+----------------------+---------------

  ;; repeat the default bindings but on modified Hat axes, as
  ;; versor-joystick will rebind the unmodified Hat axes to be its
  ;; current dimension

  (global-set-key [ ToBt2-BaBt-Hat0X-previous ] 'backward-char)
  (global-set-key [ ToBt2-BaBt-Hat0X-next ] 'forward-char)
  (global-set-key [ ToBt2-BaBt-Hat0Y-previous ] 'previous-line)
  (global-set-key [ ToBt2-BaBt-Hat0Y-next ] 'next-line)
  (global-set-key [ ToBt2-BaBt-ThumbBtn-up ] 'delete-char)
  (joystick-label-modified "ToBt2-BaBt-" "Chars/Lines" device)

  ;; -----+------------------------+----------------------+---------------
  ;; ['.] |       PiBt-     BaBt2- | transpose(v)         | insert(v)
  ;; -----+------------------------+----------------------+---------------

  ;; -----+------------------------+----------------------+---------------
  ;; [.'] | ToBt2-          BaBt2- | alter(v)             | alter(v)
  ;; -----+------------------------+----------------------+---------------

  (global-set-key [ ToBt2-BaseButton2-down ] 'versor-begin-altering-item)
  (global-set-key [ ToBt2-BaBt2-Hat0X-previous ] 'versor-alter-item-prev)
  (global-set-key [ ToBt2-BaBt2-Hat0X-next ] 'versor-alter-item-next)
  (global-set-key [ ToBt2-BaBt2-Hat0Y-previous ] 'versor-alter-item-over-prev)
  (global-set-key [ ToBt2-BaBt2-Hat0Y-next ] 'versor-alter-item-over-next)
  (global-set-key [ ToBt2-BaseButton2-up ] 'versor-end-altering-item)
  (joystick-label-modified "ToBt2-BaBt2" "Altering" device)

  ;; -----+------------------------+----------------------+---------------
  ;; [.:] | ToBt2-     BaBt-BaBt2- |                      |
  ;; -----+------------------------+----------------------+---------------

  ;; -----+------------------------+----------------------+---------------
  ;; [':] |       PiBt-BaBt-BaBt2- |                      |
  ;; -----+------------------------+----------------------+---------------

  ;; -----+------------------------+----------------------+---------------
  ;; [:.] | ToBt2-PiBt-BaBt-       |                      |
  ;; -----+------------------------+----------------------+---------------

  ;; -----+------------------------+----------------------+---------------
  ;; [:'] | ToBt2-PiBt-     BaBt2- |                      |
  ;; -----+------------------------+----------------------+---------------

  ;; -----+------------------------+----------------------+---------------
  ;; [::] | ToBt2-PiBt-BaBt-BaBt2- |                      |
  ;; -----+------------------------+----------------------+---------------

  ;; modal etc settings: the minibuffer
  (mapcar (lambda (map)
	    (define-key map [ Trigger-up ] 'exit-minibuffer)
	    (define-key map [ TopBtn-up ] 'minibuffer-complete)
	    )
	  (list minibuffer-local-map
		minibuffer-local-must-match-filename-map
		minibuffer-local-filename-completion-map
		minibuffer-local-completion-map
		minibuffer-local-must-match-map
		minibuffer-local-ns-map))

  (global-set-key [ BaseBtn3-down ] 'joystick-help)
  (global-set-key [ PiBt-BaseBtn3-down ] 'joystick-help)
  (global-set-key [ BaBt-BaseBtn3-down ] 'joystick-help)
  (global-set-key [ BaBt2-BaseBtn3-down ] 'joystick-help)
  (global-set-key [ BaseBtn3-up ] 'recenter)
  (global-set-key [ PiBt-BaseBtn3-up ] 'recenter)
  (global-set-key [ BaBt-BaseBtn3-up ] 'recenter)
  (global-set-key [ BaBt2-BaseBtn3-up ] 'recenter)

  ;; All shoulder buttons except last with Hat changes the window size
  (global-set-key [ ToBt2-PiBt-BaBt-Hat0X-previous ] 'shrink-window-horizontally)
  (global-set-key [ ToBt2-PiBt-BaBt-Hat0X-next ] 'enlarge-window-horizontally)
  (global-set-key [ ToBt2-PiBt-BaBt-Hat0Y-previous ] 'shrink-window)
  (global-set-key [ ToBt2-PiBt-BaBt-Hat0Y-next ] 'enlarge-window)
  (joystick-label-modified "ToBt2-PiBt-BaBt" "Window size" device)

  ;; Right-hand small fiddly button changes the speed
  (global-set-key [ BaseBtn4-Hat0Y-previous ] 'joystick-faster)
  (global-set-key [ BaseBtn4-Hat0Y-next ] 'joystick-slower)
  (joystick-label-modified "BaseBtn4" "Repeat speed" device)
  ;; by itself, right-hand small fiddly button undoes
  (global-set-key [ BaseBtn4-up ] 'undo)

  ;; if you realize you've got the wrong buttons pressed, and want to
  ;; cancel the gesture without anything happening, press the all the
  ;; shoulder buttons; release the other buttons first, then all the
  ;; shoulder ones, the last shoulder one to go down should be the first to come up
  (global-set-key [ ToBt2-PiBt-BaBt-BaBt2-Trigger-up ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-PiBt-BaBt-BaBt2-ThumbBtn-up ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-PiBt-BaBt-BaBt2-TopBtn-up ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-PiBt-BaBt-BaBt2-ThumbBtn2-up ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-PiBt-BaBt2-BaseBtn-release ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-PiBt-BaBt2-BaseBtn-up ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-PiBt-BaBt-BaseBtn2-release ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-PiBt-BaBt-BaseBtn2-up ] 'joystick-do-nothing)
  (global-set-key [ PiBt-BaBt-BaBt2-TopBtn2-release ] 'joystick-do-nothing)
  (global-set-key [ PiBt-BaBt-BaBt2-TopBtn2-up ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-BaBt-BaBt2-PinkieBtn-release ] 'joystick-do-nothing)
  (global-set-key [ ToBt2-BaBt-BaBt2-PinkieBtn-up ] 'joystick-do-nothing)

  (joystick-label-modified "" "Default" device)

  (run-hook-with-args 'joystick-bindings-hook device))

;; Make the bindings when the joystick starts, because the help system
;; can set itself up more fully when it has the button numbering
;; information back from the joystick:
(add-hook 'joystick-initialized-hook 'joystick-gamepad-setup)

(provide 'joystick)

;;; joystick.el ends here
