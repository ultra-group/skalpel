;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2009, 2010, 2011 Heriot-Watt University
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; * Authors: Joe Wells
;; * Affiliation: Heriot-Watt University, MACS
;; * Date: 2009
;; * Description: **TO BE ADDED
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump-slices ()
"Goes through all the slices that are currently loaded in Emacs and extracts the constructed 'skalpel-info information from them (by getting it from the first overlay of each slice)."
  (mapcar
   (lambda (ovs)
     (if (null ovs)
	 "bogus null slice (why is this in skalpel-slices anyway?)"
       (concat (append (overlay-get (nth 0 ovs) 'skalpel-info) nil))))
   skalpel-slices))

;;(pp skalpel-slices)
;;(dump-slices)

(defun log-window-info (tag)
"Records on window-info-log 5 pieces of data for each active window (i.e., pane) on the selected Emacs frame (i.e., top-level X window). Used while debugging the code that rearranges windows (panes)."
  (or (boundp 'window-info-log)
      (setq window-info-log nil))
  (push (cons tag
	      (mapcar
	       (lambda (w)
		 (list w
		       (window-buffer w)
		       (window-start w)
		       (window-point w)
		       (with-current-buffer (window-buffer w) (point))
		       ))
	       (window-list)))
	window-info-log))

;;(setq window-info-log nil)
;;(pp window-info-log)

;; Idea doesn't really work in practice due to keyboard focus being messed up:
;;(defun edebug-get-buffer-window (buffer)
;;  (get-buffer-window buffer 'visible))
