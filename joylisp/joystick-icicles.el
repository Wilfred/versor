;;; joystick-icicles.el --- enable the joystick for icicles

;; Copyright (C) 2007  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: hardware, convenience

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

;; Key bindings to use the hat switch for cycling through possible
;; completions

;;; Code:

(require 'joystick)

(defun bind-joystick-icicles-minibuffer-keys ()
  "Make the minibuffer key-bindings needed to use icicles via the joystick."
  (let ((bind-c-k (lambda (this-map)
		    (define-key this-map [ Hat0X-previous ] 'icicle-previous-prefix-candidate)
		    (define-key this-map [ Hat0X-next ] 'icicle-next-prefix-candidate)
		    (define-key this-map [ Hat0Y-previous ] 'icicle-previous-apropos-candidate)
		    (define-key this-map [ Hat0Y-next ] 'icicle-next-apropos-candidate))))

    (funcall bind-c-k minibuffer-local-completion-map)

    ;; `minibuffer-local-must-match-map` ': must-match map.
    ;; In Emacs 22+, local-completion is parent of local-must-match
    (unless (eq minibuffer-local-completion-map
		(keymap-parent minibuffer-local-must-match-map))
      (funcall bind-c-k minibuffer-local-must-match-map))))

(defun joystick-icicles-setup ()
  "Making joystick bindings for icicles."
  (interactive)
  (require 'icicles)
  (bind-joystick-icicles-minibuffer-keys)
  (global-set-key [ BaBt-BaseBtn2-up ] 'icicle-execute-extended-command)
  (global-set-key [ PiBt-BaseBtn2-up ] 'icicle-find-file)
  (global-set-key [ BaBt2-PinkieBtn-up ] 'icicle-buffer)
  )

(provide 'joystick-icicles)
;;; joystick-icicles.el ends here
