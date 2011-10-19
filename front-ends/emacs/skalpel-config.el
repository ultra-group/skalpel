;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2009, 2010  Steven Shiells
;; Copyright 2009, 2010  Heriot-Watt University
;; Copyright 2011 John Pirie
;; Copyright 2011 Scott Fotheringham
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
;; * Authors: Joe Wells, Steven Shiells
;; * Affiliation: Heriot-Watt University, MACS
;; * Date: 10 November 2009
;; * Description:
;;     Emacs configuration file that loads the files to integrate
;;     Skalpel in Emacs. It also declares key bindings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;**(defvar skalpel-emacs-directory "/usr/local/share/emacs/site-lisp/skalpel-emacs" "The location where the user interface files are stored")
;;**(defvar skalpel-bin-directory   "/usr/local/bin" "The location where the binary of Skalpel is stored")
;;**(defvar skalpel-lib-directory   "/usr/local/share/skalpel" "The location where additional files required by Skalpel and the user interface are stored")

(defvar skalpel-developer nil "Tracks whether or not the user is a developer of Skalpel")
;;
;; Load the necessary files for the Emacs interface
;;
;; ** NEED TO RENAME FILES TO SOMETHING BETTER

(load (expand-file-name
       "skalpel-main.el"
       skalpel-emacs-directory))

(load (expand-file-name
       "skalpel-menu.el"
       skalpel-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following are suggested key bindings to make the operation of
;; Skalpel quicker. Feel free to change the bindings if you wish.

(defun skalpel-define-keybindings-in-map (map)
  "Sets up key bindings for functions to use skalpel"
  ;; Make "F6" run skalpel.
  (define-key map [f6] 'skalpel-run-slicer-exec)
  ;; Make "F7" display the next error slice.
  (define-key map [f7] 'skalpel-next-slice)
  ;; Make "Shift-F7" display the next part of the slice currently in focus.
  (define-key map [S-f7] 'skalpel-next-part-of-slice)
  ;; Make "F8" display the previous error slice.
  (define-key map [f8] 'skalpel-prev-slice)
  ;; Make "F9" remove all of the error slices from the current file.
  (define-key map [f9] 'skalpel-forget-all-slices-file)
  ;; Make "F10" remove all of the error slices from all files.
  (define-key map [f10] 'skalpel-forget-all-slices)
  ;; Make "F11" display the help buffer.
  (define-key map [f11] 'skalpel-show-help)
  ;; Control-c Control-p runs Skalpel, then compiler (if no errors are found by Skalpel)
  (define-key map "\C-c\C-p" 'skalpel-run-slicer-then-compiler))

(eval-after-load "sml-mode"
  '(skalpel-define-keybindings-in-map sml-mode-map))

;; Next item works with a particular version of the SML mode but I'm
;; not sure that inferior-sml-mode-map is always defined in sml-proc.
(eval-after-load "sml-proc"
  '(skalpel-define-keybindings-in-map inferior-sml-mode-map))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
