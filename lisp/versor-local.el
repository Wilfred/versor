;;;; versor-local.el -- select navigation dimensions per mode or per buffer
;;; Time-stamp: <2003-10-16 16:54:18 john>

(provide 'versor-local)
(require 'versor)
(require 'buffer-select-hooks)

(defvar versor:per-buffer nil
  "*Remember the dimensions per buffer")

(defvar versor:this-buffer-meta-level nil
  "The remembered meta-level for this buffer.")

(defvar versor:this-buffer-level nil
  "The remembered level for this buffer.")

(make-variable-buffer-local 'versor:this-buffer-meta-level)
(make-variable-buffer-local 'versor:this-buffer-level)

(defvar versor:auto-change-for-modes t
  "*Whether to change the dimension on changing modes.")

(defun versor:mode-change-function (old-mode)
  "Select dimension if necessary, as a hook on changing modes."
  (if (and versor:auto-change-for-modes
	   (not versor:per-buffer))
      )
)

(defun versor:buffer-change-function (old-buffer)
  "Select dimension if necessary, as a hook on changing buffers."
  (if versor:per-buffer
      (progn
	(save-excursion
	  (set-buffer old-buffer)
	  (setq versor:this-buffer-meta-level versor:meta-level
		versor:this-buffer-level versor:level))
	(versor:select-named-meta-level versor:this-buffer-meta-level)
	(versor:select-named-level versor:this-buffer-level))))

(add-hook 'mode-selection-hook 'versor:mode-change-function)
(add-hook 'buffer-selection-hook 'versor:buffer-change-function)

