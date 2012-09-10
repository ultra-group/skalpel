;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2009, 2010 Steven Shiells
;; Copyright 2009, 2010, 2011, 2012 Heriot-Watt University
;; Copyright 2010 John Pirie
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
;; You should have received a copy of the GNU General Public License
;; along with skalpel-emacs.  If not, see <http://www.gnu.org/licenses/>.
;; * Authors:  Joe Wells, Steven Shiells, Vincent Rahli, Scott Fotheringham, John Pirie
;; * Affiliation: Heriot-Watt University, MACS
;; * Date: 10 November 2009
;; * Description:
;;     Main functions to interact with skalpel:
;;     run skalpel on a piece of code, switch from one slice to another, ...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; require Common Lisp package
;; do we actually need this? I don't think we do. Scott.
(require 'cl)

;; a function to safely load files. If load-file fails, an exception
;; is thrown and we return
(defmacro safe-load-file (fn)
  `(unwind-protect
       (condition-case ex
	   (progn ,fn)
	 (error
	  (message "Error while loading %s: %S" output-file ex)
	  ;; *** This is the wrong way to solve things, but mostly
	  ;; will work.  Occasionally this will blow us some other
	  ;; user code that happens to be on the call stack when the
	  ;; timer runs, so we should fix this at some point.
	  (throw 'i-just-made-this-symbol-up nil)
	  ))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;                            FACES                              ;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; STANDARD ERROR: FOCUS AND NON FOCUS

;; *** need better name ***
(defface skalpel-standard-error-focus
  '((t (:background "#ff0000" :foreground "#fefefe")))
  "Used to highlight a section of code that contributes to the type error when the error is in focus.")

;; *** need better name ***
(defface skalpel-standard-error-non-focus
  '((t (:background "#ffdcbd" :foreground "#000000")))
  "Used to highlight a section of code that contributes to the type error when the error is not in focus.")



;;;; STANDARD ERROR BOX: FOCUS AND NON FOCUS

(defface skalpel-standard-error-box-focus
  '((t (:box (:line-width 2 :color "#ff0000" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is in focus).")

(defface skalpel-standard-error-box-non-focus
  '((t (:box (:line-width 2 :color "#ffdcbd" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is not in focus).")



;;;; STANDARD ERROR HEAD: FOCUS

(defface skalpel-standard-error-head-focus
  '((t (:background "#ff8e8e" :foreground "white" :weight extra-bold)))
  "*********************************************")



;;;; END POINT ONE: FOCUS AND NON FOCUS

(defface skalpel-end-point-one-focus
  '((t (:background "#5e76c1" :foreground "#ffffff")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (in focus).")

(defface skalpel-end-point-one-non-focus
  '((t (:background "#dfdffb")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (not in focus).")



;;;; END POINT ONE BOX: FOCUS AND NON FOCUS

(defface skalpel-end-point-one-box-focus
  '((t (:box (:line-width 2 :color "#5e76c1" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is in focus).")

(defface skalpel-end-point-one-box-non-focus
  '((t (:box (:line-width 2 :color "#dfdffb" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is not in focus).")



;;;; END POINT ONE HEAD: FOCUS

(defface skalpel-end-point-one-head-focus
  '((t (:background "#9999ff" :foreground "white" :weight extra-bold)))
  "********************************************")



;;;; END POINT TWO: FOCUS AND NON FOCUS

(defface skalpel-end-point-two-focus
  '((t (:background "#7b7a85" :foreground "#ffffff")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (in focus).")

(defface skalpel-end-point-two-non-focus
  '((t (:background "#e3e3e3")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (not in focus).")



;;;; END POINT TWO BOX: FOCUS AND NON FOCUS

(defface skalpel-end-point-two-box-focus
  '((t (:box (:line-width 2 :color "grey" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is in focus).")

(defface skalpel-end-point-two-box-non-focus
  '((t (:box (:line-width 2 :color "#e3e3e3" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is not in focus).")



;;;; END POINT TWO HEAD: FOCUS

(defface skalpel-end-point-two-head-focus
  '((t (:background "grey" :foreground "white" :weight extra-bold)))
  "**********************************************")



;;;; MERGED ERROR: FOCUS AND NON FOCUS

(defface skalpel-merged-regions-focus
  '((t (:background "#af1de2" :foreground "#ffffff")))
  "Used to highlight a section of code that is an end point of a record clash (in focus). The highlited text appears in both clashing records.")

(defface skalpel-merged-regions-non-focus
  '((t (:background "#d8b1e2")))
  "Used to highlight a section of code that is an end point of a record clash (not in focus). The highlited text appears in both clashing records.")



;;;; MERGED ERROR BOX: FOCUS AND NON FOCUS

(defface skalpel-merged-regions-box-focus
  '((t (:box (:line-width 2 :color "af1de2" :style nil))))
  "Used to surround a piece of code in a tuple clashing with a record.
  The piece of code is the the nth component of the tuple but the record does not mention n")

(defface skalpel-merged-regions-box-non-focus
  '((t (:box (:line-width 2 :color "#d8b1e2" :style nil))))
  "Used to surround a piece of code in a tuple clashing with a record.
  The piece of code is the the nth component of the tuple but the record does not mention n")



;;;; FURTHER EXPLANATION: FOCUS AND NON FOCUS

;; *** need better name ***
(defface skalpel-further-explanations-focus
  '((t (:background "#FF9300")))
  "Used to highlight code provides information about the status of identifiers or that is expansive (in focus).")

;; *** need better name ***
(defface skalpel-further-explanations-non-focus
  '((t (:background "#FFBF77")))
  "Used to highlight code provides information about the status of identifiers or that is expansive (not in focus).")



;;;; FURTHER EXPLANATION BOX: FOCUS AND NON FOCUS

(defface skalpel-further-explanations-box-focus
  '((t (:box (:line-width 2 :color "#FF20AA" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is in focus).")

(defface skalpel-further-explanations-box-non-focus
  '((t (:box (:line-width 2 :color "#FFB3E1" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is not in focus).")



;;;; PARSING: FOCUS AND NON FOCUS

(defface skalpel-parsing-error-focus
  '((t (:background "#FFE50B")))
  "Used to highlight the regions of code that cannot be parsed by Skalpel.")

(defface skalpel-parsing-error-non-focus
  '((t (:background "#FFE50B")))
  "Used to highlight the regions of code that cannot be parsed by Skalpel.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;                          VARIABLES                            ;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar skalpel-fake-lines 0 "Variable to keep track of the number lines added to the file being sliced")

;; Variable to keep track of the number of errors found by skalpel
;; *** DEAD VARIABLE?
(defvar skalpel-item-count 0 "Variable to keep track of the number of errors found by skalpel")

;; Variable to keep track of the number of errors processed
(defvar skalpel-error-count 1 "Variable to keep track of the number of errors processed")

;; The time limit that skalpel is allowed to run
;; 1 hour by default
(defvar skalpel-timelimit 3600000 "The time limit that Skalpel is allowed to run")

;; *** fix to use expand-file-name and test
(defvar skalpel-basis-file (expand-file-name "basis.sml" skalpel-lib-directory) "Variable to keep track of the SML/NJ basis file")

;; *** should be named skalpel-sml-program (“program”, not “process”)
;; sml
;; (defvar skalpel-sml-process "/home/vince/Work/smlnj-11070/bin/sml")
(defvar skalpel-sml-process "sml" "The command used to run sml")

;; the option concerning the use of the basis
;; 0: no basis
;; 1: buildin environment
;; 2: basis file
(defvar skalpel-basis-option 2 "Variable to keep track of the basis option used by Skalpel")

;; variable from steven
(defvar skalpel-auto-display-entire-slice nil)

;; Variable to flag when the user wishes to have the verbose error messages displayed
(defvar skalpel-verbose-error-messages t "Variable to flag when the user wishes to have the verbose error messages displayed")

;; set to true if the user wishes to automatically see slice information in another buffer
(defvar skalpel-show-slice t "Variable to flag when the user wishes to automatically see slice information in another buffer")

;; set to true if the user wants to hide basis information during overloading errors
(defvar skalpel-show-basis nil "Variable to flag when the user wishes to show basis information during overloading errors ")

;; a function from from slide id to the point markers associated with that slice id (for removing old slices)
(defvar skalpel-slice-info-slices "function from slice id to pair of point locations")

;;(defvar skalpel-slices-processed-mark)
;;
;;(put 'skalpel-slices-processed-mark 'permanent-local t)

;; ???
(defvar skalpel-slice-data nil)

(if (eq system-type 'windows-nt)
    (defvar skalpel-slicer-bin "skalpel.exe")
    (defvar skalpel-slicer-bin "skalpel")
    )

;; Size of a tabulation
;; (2010-03-24)We can add that as an argument to the Skalpel binary
(defvar skalpel-tab-size default-tab-width)

;; Variable to hold the message about the success or failure of Skalpel
(defvar skalpel-finished-message nil "Variable to hold the message about the success or failure of Skalpel")

;; Variable to hold the list of slices found by Skalpel
(defvar skalpel-slices nil "Variable to hold the list of slices found by Skalpel")

;; Variable to hold the list of slices found by Skalpel that have been viewed by the user
(defvar skalpel-slices-previous nil "Variable to hold the list of slices found by Skalpel that have been viewed by the user")

;; Variable to hold the list of overlays that are used to highlight sections of the source file
(defvar skalpel-overlays nil "Variable to hold the list of overlays that are used to highlight sections of the source file")

;; Variable to keep track of the current file that is being sliced
(defvar skalpel-file-being-sliced nil "Variable to keep track of the current file that is being sliced by Skalpel")

;; Variable to point to the current slice
(defvar skalpel-current-slice-pointer nil "Variable to point to the current slice")

;; Variable to keep track of the parts of the current slice which have not been viewed
(defvar skalpel-current-slice-not-viewed nil "Holds the ")

;; Name of the temporary directory that will contain all the temporary files generated by Skalpel
(defvar skalpel-temporary-directory nil "Name of the temporary directory that will contain all the temporary files generated by Skalpel")

;; Identifier of the slice currently highlighted
(defvar skalpel-current-highlighted-slice nil "Identifier of the slice currently highlighted")

;; Keep temporary files between every time slicer runs
(defvar skalpel-keep-temporary-files-for-debugging t
  "If non-nil, then temporary files used for communication between
Emacs and Skalpel back end are not deleted.  This makes
debugging easier.  In normal use, this variable should have the
value nil.")

;; Advice that allows the properties of tooltips to be manipulated so
;; that the error messages are displayed properly
(defadvice tooltip-show (around fix-stupid-face-idiocy activate)
  (let ((tooltip-show-original-text (ad-get-arg 0)))
    ad-do-it))

;; Advice that allows the properties of tooltips to be manipulated so
;; that the error messages are displayed properly
(defadvice x-show-tip (before fix-stupid-face-idiocy activate)
  (if (boundp 'tooltip-show-original-text)
      (ad-set-arg 0 tooltip-show-original-text))
  )

;; Define variable for user's preferred compiler, default to SML/NJ
(defvar skalpel-user-compiler-smlnj nil)
(defvar skalpel-user-compiler-mlton nil)
(defvar skalpel-user-compiler-polyml nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;                          FUNCTIONS                            ;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun skalpel-unread-only-basis ()
  "Makes the basis file writable if it is read-only"
  (let ((cur-buf (current-buffer)))
    (set-buffer (get-file-buffer skalpel-basis-file))
    (if buffer-read-only
	(toggle-read-only)
      )
    (set-buffer cur-buf)
    )
  )

;; *** We should just use (make-temp-file "skalpel" t) instead!  Arrgh!
(defun skalpel-set-temporary-directory ()
  "Sets the skalpel-temporary-directory to a new unused temporary file name"
  ;; We might want to use the tmpdir variable instead
  (let* ((skalpel-tmp (if (eq system-type 'windows-nt) "C:\\temp\\skalpel" "/tmp/skalpel"))
	 (rand (random))
	 (file nil))
    (if (> 0 rand) (setq rand (- rand)))
    (setq file (format "%s-%d" skalpel-tmp rand))
    (while (file-exists-p file)
      (setq rand (+ rand 1))
      (setq file (format "%s-%d" skalpel-tmp rand))
      )
    (setq skalpel-temporary-directory file)
    )
  )

(defun skalpel-get-temporary-directory ()
  "Builds the name of the temporary directory used by Skalpel"
  ;;(format "%s-slices" skalpel-file-being-sliced)
  (if (not skalpel-temporary-directory)
      ;; This should never happen
      (skalpel-set-temporary-directory))
  skalpel-temporary-directory
  )

(defun skalpel-delete-temporary-directory ()
  "Deletes the temporary directory and files produced by Skalpel if they exist"
  (let ((temp-dir (skalpel-get-temporary-directory)))
    (if (file-exists-p temp-dir)
	;; *** Eeek!  Running rm -rf is dangerous!!!  We have not
	;; really validated temp-dir.  If
	;; (skalpel-get-temporary-directory) can ever return a bad
	;; value, we might remove all the user's files (or the entire
	;; system if running as root)!
	(or skalpel-keep-temporary-files-for-debugging
	    (shell-command (format "rm -rf %s" temp-dir)))
      )
    )
  )

(defun skalpel-stop-skalpel-process ()
  (interactive)
  "Stops the skalpel process if it exists"
  (let ((proc (get-process "skalpel")))
    (if (not (equal nil proc))
	(delete-process proc)
      )
    )
  )

(defun check-valid-file ()
  (if (buffer-modified-p (get-file-buffer skalpel-file-being-sliced)) ;; if unsaved file, abort
      (progn (skalpel-trace "FILE UNSAVED - SLICING ABORTED") nil)
    (if (string-equal skalpel-file-being-sliced skalpel-basis-file) ;; if basis file, abort
	(progn (skalpel-trace "BASIS FILE - SLICING ABORTED") nil)
      t)
  ))

(defun pre-slicing-setup ()
  "Shows slice buffer to the user, Sets skalpel-[item-count; error-count; file-being-sliced]"
  (progn
    ;; create the buffer where we place program slices
    (get-buffer-create "*skalpel-slice-info*")
    (if skalpel-show-slice ;; if user auto-shows slices, display them
	(display-buffer "*skalpel-slice-info*" pop-up-windows))
    (with-current-buffer "*skalpel-slice-info*" (erase-buffer))

    ;; set variables
    (setq skalpel-item-count 0) ;; set the number of found errors to 0
    (setq skalpel-error-count 1) ;; set skalpel-error-count to 1 as slicer produces files numbered 1,2,...
    (setq skalpel-file-being-sliced (buffer-file-name)) ;; keep track of the the file being sliced

    (if (buffer-modified-p (get-file-buffer skalpel-file-being-sliced))
	(save-some-buffers)
      )

    (if (equal skalpel-show-basis nil)
	(setq skalpel-show-basis 0)
      (setq skalpel-show-basis 1))

    ))

(defun skalpel-run-slicer ()
  "Runs Skalpel using 'commslicerp' function (sml/nj compiles sources)"
  (interactive)

  (pre-slicing-setup)

  (if (not (check-valid-file)) ()
      (save-window-excursion
      	;; Set up the command to run Skalpel
      	(let ((cur-file (file-name-nondirectory skalpel-file-being-sliced))
      	      (make-command (format "echo 'CM.make \"%s\"; "
      				    (expand-file-name "sources.cm"
      						      skalpel-sources-directory)))
      	      (open-command "open Slicer; ")
      	      (run-command (format "commslicerp \"%s\" [\"%s\"] \"\" \"\" \"\" \"" skalpel-basis-file skalpel-file-being-sliced))
      	      (output-file nil)
      	      (output-dir nil)
      	      (rest-command (format "\" \"\" %d %d;' | %s" skalpel-basis-option skalpel-timelimit skalpel-sml-process))
	      )
      	  (skalpel-set-temporary-directory)
      	  (setq output-dir (skalpel-get-temporary-directory))
      	  (setq output-file (expand-file-name (format "%s.el" cur-file) output-dir))

      	  ;; Format the command
      	  (setq skalpel-command (concat make-command open-command run-command output-file rest-command))

      	  (skalpel-forget-all-slices) ;; remove all of the error information in the current file
      	  (shell-command (format "mkdir %s" output-dir)) ;; create temp dir
      	  (skalpel-trace "RUNNING SLICER: please wait..") ;; run Skalpel
      	  (skalpel-stop-skalpel-process) ;; kill skalpel-process

      	  (start-process-shell-command "skalpel" "*skalpel-debugging-output*" skalpel-command) ;; start skalpel process

      	  ;; Process the errors
      	  (skalpel-process-errors)))))

(defun skalpel-run-slicer-exec ()
  "Runs Skalpel using the pre-built binary."
  (interactive)

  (if (equal (file-executable-p (concat skalpel-bin-directory "/" skalpel-slicer-bin)) nil)
      (skalpel-trace (concat "Error: Skalpel binary couldn't be found or wasn't executable in " skalpel-bin-directory "/" skalpel-slicer-bin))
      (progn
	(pre-slicing-setup)
	(if (not (check-valid-file)) () (progn
	    (save-window-excursion
	      ;; Set up the command to run Skalpel
	      (let* ((cur-file (file-name-nondirectory skalpel-file-being-sliced))
		     (output-dir (progn
				   (skalpel-set-temporary-directory)
				   (skalpel-get-temporary-directory)))
		     (output-file (expand-file-name (format "%s.el" cur-file) output-dir))
		     (slicer-command
		      (format "%s -b %d %s -l %s -t %d -min true -bo %d %s"
		      	      (shell-quote-argument (expand-file-name skalpel-slicer-bin skalpel-bin-directory)) ;; arg0
		      	      skalpel-basis-option ;; arg1
		      	      (shell-quote-argument skalpel-basis-file) ;; arg2
		      	      (shell-quote-argument output-file) ;; arg4
		      	      skalpel-timelimit ;; arg5
		      	      skalpel-show-basis
		      	      (shell-quote-argument skalpel-file-being-sliced) ;; arg3
		      	      )) ;; arg6

		     (run-command
		      (concat
		       "echo '';"
		       "echo ======================================================================;"
		       "date;"
		       "echo '';"
		       (format "echo running this command: %s;" (shell-quote-argument slicer-command))
		       "echo '';"
		       (if (eq system-type 'windows-nt)
			   ()
			 (format "env nice -n 40 %s;" slicer-command)  ;; force nice program (not the command!). 40 is max value on POSIX machines
			 )
		       "echo '';"
		       "date;")
		      )
		     (debug-output-buffer "*skalpel-debugging-output*")
		     (timer nil) ;; *** DEAD VARIABLE?
		     )
		(skalpel-forget-all-slices) ;; remove error info loaded previously
		;; *** Use make-directory instead!  Arrgh!
		(shell-command (format "mkdir %s" output-dir)) ;; create temp dir
		(skalpel-trace "RUNNING SLICER: Please wait..")
		(setq skalpel-last-run-command run-command) ;; debugging trace
		(skalpel-stop-skalpel-process) ;; kill skalpel- process
		;; Ensure we scroll if otput being watched in a window
		;; When previous subprocess terminated, a message is appended to the buffer after point, so we need to skip this.
		(with-current-buffer (get-buffer-create debug-output-buffer)
		  (goto-char (point-max)) ;; not sure it has desired effect
		  (let ((w (get-buffer-window (current-buffer) t))) ;; does the real work
		    (when w
		      (set-window-point w (point))
		      ;; Ensure the frame is visible
		      (let ((f (window-frame w)))
			(if (memq (frame-visible-p f) '(nil icon))
			    (raise-frame f)))))) ;; maybe we should just make-frame-visible? We really should check which desktop this is on

		(start-process-shell-command "skalpel" debug-output-buffer slicer-command) ;; start a new skalpel process (there can only be one such process)
		(skalpel-process-errors))))))))

(defun skalpel-process-errors ()
  "This function processes the output files produced by Skalpel"
  ;; Get the file path to the containing directory and the file name of the file being sliced

  ;; there should be a better way of doing this- fix
  (if (equal skalpel-show-basis 0)
      (setq skalpel-show-basis nil)
    ())

  (let ((cur-file (file-name-nondirectory skalpel-file-being-sliced))
	(output-file nil)
	(finished-file nil)
	(curid skalpel-current-highlighted-slice))
    ;; Create the name of the output files
    (setq output-file (expand-file-name (format "%s-%d.el" cur-file skalpel-error-count)
					(skalpel-get-temporary-directory)))
    (setq finished-file (expand-file-name (format "%s-finished.el" cur-file)
					  (skalpel-get-temporary-directory)))
    ;; If the file indicating Skalpel has finished exists ([filename-finished].el), and there are no other error files to be processed then ...
    (if (and (file-exists-p finished-file)
	     (not (file-exists-p output-file)))
	(progn
	  ;; ... Remove all of the temporary files produced by Skalpel ...
	  (safe-load-file (load-file finished-file))
	  (skalpel-delete-temporary-directory)
	  (skalpel-trace (concat "SLICING FINISHED WITH STATUS: " skalpel-finished-message)))
      ;; ... else when there are no slices currently getting processed
      ;; and the next error file exists, then process the next error
      (if (file-exists-p output-file)
	  (progn
	    (safe-load-file (load-file output-file))
	    (if ;; *** This should test *ALL* the buffers containing
		;; files Skalpel is working on as input!  BUG!
		(buffer-modified-p (get-file-buffer skalpel-file-being-sliced))
		(progn
		  ;; *** We should also kill the skalpel process!
		  ;; Otherwise it will continue to run using oodles of
		  ;; CPU time and memory.
		  (skalpel-delete-temporary-directory)
		  (skalpel-trace "BUFFER MODIFIFED - SLICING STOPPED"))
	      ;; Process the error
	      (skalpel-process-error-file skalpel-slice-data)
	      ;; Remove the error file that has just been processed
	      (or skalpel-keep-temporary-files-for-debugging
		  (shell-command (format "rm %s " output-file)))
	      ;; Increment the counter that keeps track of the next error
	      (setq skalpel-error-count (1+ skalpel-error-count))
	      (setq skalpel-current-slice-pointer (butlast skalpel-slices))
	      ;; If an error was highlighted and has been replaced, we highlight the new one
	      (if skalpel-current-highlighted-slice
		  (if (not (equal skalpel-current-highlighted-slice curid))
		      (skalpel-next-slice-by-id skalpel-current-highlighted-slice)
		    )
		)
	      (skalpel-trace "SEARCHING FOR OUTPUT: Please wait..")
	      ;; The next error is ready so process it
	      (skalpel-process-errors)))
	;; Still waiting on Skalpel, so wait a bit before trying again
	(progn
	  (if (get-process "skalpel") ;; if Skalpel is still running
	      (run-at-time 1 nil 'skalpel-process-errors) ;; keep looking for errors
	      (run-at-time 1 nil 'skalpel-process-errors) ;; keep looking for errors
	      ;; (skalpel-trace "Skalpel encountered an unknown internal bug") ;; otherwise report that it has died
	    ))
	))))

(defun skalpel-process-error-file (list)
  "Processes the information in the file containing the details of the error"
  (let ((count 0))
    ;; Keep track of the number of errors in the file
    ;; ** Should only ever be 1 error per file, but the function allows for more (just incase) **
    (setq skalpel-item-count (safe-length list)) ;; *** DEAD VARIABLE?
    ;; While there are to process
    (while (< count (safe-length list))
      (let ((item (nth count list)))
	;; Identify the different parts of the error
	(let* ((id          (cdr (assoc 'id item)))
	       (regs        (cdr (assoc 'regions item)))
	       (slice       (cdr (assoc 'slice item)))
	       (assumptions (cdr (assoc 'assumptions item)))
	       (kind        (cdr (assoc 'kind item)))
	       (remove      (cdr (assoc 'remove item))))

	  (if (equal regs nil)
	      (progn (skalpel-trace "ERROR: No regions to highlight for this code!")
		     (return)))

	  ;; Highlight the slice
	  ;; *** pass these as separate arguments!!!
	  (skalpel-highlight-slice (cons regs (cons (skalpel-format-slice-info slice assumptions kind) (cons id nil))))

	  ;; foucs the first slice
	  (skalpel-next-slice-by-id 0)

	  ;; Some slices may require some other slices to be removed (slices that are two or more minimal slices merged)
	  (when (not (equal 0 (safe-length remove)))
	    ;; Remove all of the slices that are no longer needed
	    (while remove
	      (if (equal (car remove) skalpel-current-highlighted-slice)
	       	  ;; The we should highlight the current slice
		  ;; It is going to be done when returning from skalpel-process-error-file in skalpel-process-error
		  (setq skalpel-current-highlighted-slice id)
		)
	      (skalpel-forget-slice-by-id (car remove))
	      (setq remove (cdr remove))))
	  ))
      ;; Increment the counter "count" so that the next error will be processed
      (setq count (1+ count))))
  )

;; *** Use this to track down errors in timer handler.
(defvar skalpel-trace-signals-log nil)

(defmacro skalpel-trace-signals (log &rest body)
  (declare (indent 2)
	   (debug (sexp sexp body)))
  `(condition-case
       ;; *** should really generate a fresh symbol for this:
       skalpel-trace-signals-data
       (progn ,@body)
     (error
      ;; log the signal
      (push skalpel-trace-signals-data ,log)
      ;; re-raise the signal
      (signal (car skalpel-trace-signals-data) (cdr skalpel-trace-signals-data)))))

;; (pp (macroexpand '(skalpel-trace-signals log-var x y z)))

(defun skalpel-highlight-slice (args)
  "Highlights a slice - a slice is made up of a number of regions"
  (skalpel-trace "HIGHLIGHTING A SLICE ENTER [%s]" (car (cddr args)))
  ;; Identify the different parts of the error
  ;; *** Start using skalpel-trace-signals.
  ;;(skalpel-trace-signals skalpel-trace-signals-log

  (let ((formattedRegs nil)
	(regs  (car args))
	(slice (car (cdr args)))
	(id    (car (cddr args)))
	(myovs nil))
    (while regs
      (let* ((file    (caar (last regs))) ;; *** why processing last item first???
	     (regions (cdr (assoc file regs))))
	(with-current-buffer (find-file-noselect file)
	  ;; Put the information into a form in which it is easily processed
	  (setq slice (cons id (cons (car slice) (cons (cons (car (cdr slice)) (cons (skalpel-contains-box regions) nil)) nil))))
	  (setq formattedRegs nil)
	  (setq formattedRegs (skalpel-mapslice formattedRegs regions slice))
	  ;; Highlight the regions
	  (let ((ovs
		 (apply #'append
			(mapcar #'skalpel-highlight-region formattedRegs))))
	    (mapc
	     (lambda (ov) (overlay-put ov 'skalpel-slice ovs))
	     ovs)
	    (setq myovs (append myovs ovs)))

	  ;;	(setq regs (cdr regs))
	  (setq regs (butlast regs))
	  ))
      )
    ;; Keep track of the error slices
    (when (not skalpel-slices)
      (push nil skalpel-slices))
    (when (not (equal myovs nil))
      (push (reverse myovs) skalpel-slices))
    )
  ;; *** matching paren for commented out paren above:
  ;;)
  (skalpel-trace "HIGHLIGHTING A SLICE EXIT"))

(defun skalpel-is-box (reg)
  "test is a regions is a box"
  (let ((ret nil))
    (case (car reg)
      ;; If the error code is "N" then it is a box
      (N (setq ret t)))
    ret))

(defun skalpel-contains-box (regs)
  "search through a list of regions to determine if there is a box"
  (let ((ret "NOBOX"))
    (while regs
      (when (skalpel-is-box (car regs))
	(setq ret "BOX"))
      (setq regs (cdr regs)))
    ret))

(defun skalpel-mapslice (regs mylist slice)
  "format the list of regions mylist into regs: from ((reg1) (reg2) ...) to ((reg1 slice) (reg2 slice) ..).
  slice is more than slices, it contains extra information"
  (while mylist
    (push (cons (car mylist) (cons slice nil)) regs)
    (setq mylist (cdr mylist))
    )
  regs
  )

;; *** MAY NEED TO ADD BUFFER AS EN EXTRA INPUT PARAMTER
(defun skalpel-line-and-char-to-buf-and-pos (line char)
  "Transforms a line number and column number into a buffer and the position that (line, char) is in the buffer"
  (prog1
      (let (file)
	(cond ((<= line skalpel-fake-lines)
	       (setq file 'fake))
	      (t
	       (setq file 'main)
	       (setq line (- line skalpel-fake-lines))))
	(save-excursion
	  (set-buffer
	   (case file
	     (fake (get-buffer-create skalpel-fake-buffer))
	     (main (current-buffer))))
	  ;;	     (main (find-file-noselect skalpel-file-being-sliced))))
	  (save-excursion
	    (goto-char (point-min))
	    (forward-line (1- line))
	    (condition-case nil
		(forward-char (1- char))
	      (error nil))
	    (cons (current-buffer)
		  (point)))))
    ))


(defun skalpel-highlight-region (args)
  "Highlights a region"
  (let ((reg   (car args))
	(id    (car (car (cdr args))))
	(slice (car (cdr (car (cdr args)))))
	(info  (car (cdr (cdr (car (cdr args)))))))
    (set-text-properties 0 (length slice) nil slice)
    (if (functionp 'font-lock-append-text-property)
	(font-lock-append-text-property 0 (length slice) 'face '(:family "fixed") slice))
    (prog1
	(destructuring-bind (type (bline bchar eline echar) color . subitems) reg
	  (let* ((buf-and-pos-beg
		  (skalpel-line-and-char-to-buf-and-pos bline bchar))
		 (beg (cdr buf-and-pos-beg))
		 (buf-and-pos-end
		  (skalpel-line-and-char-to-buf-and-pos eline echar))
		 (end (cdr buf-and-pos-end))
		 (buf (car buf-and-pos-beg))
		 (ov (make-overlay
		      beg
		      (1+ end)
		      (car buf-and-pos-beg)
		      t nil)))
	    (or (eq buf (car buf-and-pos-end))
		(error "impossible"))
	    ;; Add the overlay to the list of overlays
	    (push ov skalpel-overlays)
	    ;; Add the necessary information to the properties of the overlays
	    (overlay-put ov 'skalpel-type   type)
	    (overlay-put ov 'skalpel-id     id)
	    (overlay-put ov 'skalpel-info   slice)
	    (overlay-put ov 'skalpel-color  color)
	    (skalpel-set-overlay-properties ov nil)
	    ;; Calculate and store the inforamtion for the more detailed information
	    ;; ***LINE BELOW CAUSES THE PROGRAM TO BE A LOT SLOWER
	    (overlay-put ov 'skalpel-more-info (skalpel-find-info info))
	    ;; If the user wishes to have the verbose error messages shown, then show them
	    ;; otherwise show the non verbose error messages
	    (if skalpel-verbose-error-messages
		(overlay-put ov 'help-echo (concat (overlay-get ov 'skalpel-info) (overlay-get ov 'skalpel-more-info)))
	      (overlay-put ov 'help-echo (overlay-get ov 'skalpel-info)))
	    (overlay-put ov 'evaporate t)
	    (cons ov
		  (when subitems
		    (apply #'append
			   (mapcar #'skalpel-highlight-region
				   (skalpel-mapslice nil (car subitems) (cons id (cons slice (cons info nil)))))))))
	  )
      )))

(defun skalpel-set-overlay-properties (ov focus)
  "Sets the properties of the overlays that make up the highlighting of the regions of the slices"
  (let* ((color (overlay-get ov 'skalpel-color))
	 (message-first-part (overlay-get ov 'skalpel-info))
	 (message-second-part (overlay-get ov 'skalpel-more-info)))

    ;; If the overlay is in focus display the information about the error
    ;; otherwise display no information
    (if (equal t focus)
	(progn
	  (when skalpel-verbose-error-messages
	    (setq message-first-part (concat message-first-part message-second-part)))
	  (overlay-put ov 'help-echo message-first-part))
      (overlay-put ov 'help-echo nil))
    (overlay-put ov 'skalpel-focus focus)
    (overlay-put ov 'face
    		 (case (overlay-get ov 'skalpel-type)
    		   (L (case color
    			((R) (if focus 'skalpel-standard-error-focus 'skalpel-standard-error-non-focus))
    			((B) (if focus 'skalpel-end-point-one-focus 'skalpel-end-point-one-non-focus))
    			((P) (if focus 'skalpel-end-point-two-focus 'skalpel-end-point-two-non-focus))
    			((G) (if focus 'skalpel-merged-regions-focus 'skalpel-merged-regions-non-focus))
    			((O) (if focus 'skalpel-further-explanations-focus 'skalpel-further-explanations-non-focus))
    			((Y) (if focus 'skalpel-parsing-error-focus 'skalpel-parsing-error-non-focus))
    			))
    		   (H (case color
    			((R) (if focus 'skalpel-standard-error-head-focus 'skalpel-standard-error-non-focus))
    			((B) (if focus 'skalpel-end-point-one-head-focus 'skalpel-end-point-one-non-focus))
    			((P) (if focus 'skalpel-end-point-two-head-focus 'skalpel-end-point-two-non-focus))
    			))
    		   (N (case color
    			((R) (if focus 'skalpel-standard-error-box-focus 'skalpel-standard-error-box-non-focus))
    			((B) (if focus 'skalpel-end-point-one-box-focus 'skalpel-end-point-one-box-non-focus))
    			((P) (if focus 'skalpel-end-point-two-box-focus 'skalpel-end-point-two-box-non-focus))
    			((G) (if focus 'skalpel-merged-regions-box-focus 'skalpel-merged-regions-box-non-focus))
    			((O) (if focus 'skalpel-further-explanations-box-focus 'skalpel-further-explanations-box-non-focus))
    			))
    		   (t (error "impossible"))))
    (overlay-put ov 'priority
		 (case color
		   ((B G O P)
		    (if focus 4 2))
		   ((R Y)
		    (if focus 3 1))))))

(defun skalpel-print-ek-desc (kind)
  "Given an error kind, this function will return a string which gives an explanation of that kind of error"
  (case (car kind) ;; car kind is the error type
    ((TYP) (insert "Additional info: Occurs when two type constructors are constrained to be equal\nbut are not, for example int = bool\n\n"))
    ((CIR) (insert "Additional info: Occurs when a type is constrained to contain itself\n(an infinite type)\n\n"))
    ((OVE) (insert "Additional info: Occurs when an identifier is overloaded and is used with a type that does not match the types specified by the overloading declaration\n\n"))
    ((ARI) (insert "Additional info: Occurs when a sequence of length N is constrained to be of\nlength N where M is not equal to N\n\n"))
    ((REC) (insert "Additional info: Occurs when two two records are constrained to be equal but a\nlabel appears in one record and does not appear in the other\n\n"))
    ((UNM) (insert "Additional info: Occurs when a structure does not declare an identifier that\nit is supposed to\n\n"))
    ((MSI) (insert "Additional info: Occurs when a constructor occurs in a structure but not in its signature\n\n"))
    ((MST) (insert "Additional info: Occurs when a constructor occurs in a signature but not in a structure with this signature\n\n"))
    ((DTC) (insert "Additional info: Occurs when a type constructor is defined as a type function in a structure and as a datatype in its signature\n\n"))
    ((PAR) (insert "Additional info: Please refer to the details above\n\n"))
    ((MUL) (insert "Additional info: Occurs when an identifier is bound more than once in one\ndeclaration\n\n"))
    ((VAL) (insert "Additional info: Occurs when a value variable is supplied an argument in a\npattern\n\n"))
    ((EXV) (insert "Additional info: Occurs when an identifier is declared as a value variable and used as an exception\n\n"))
    ((EXD) (insert "Additional info: Occurs when an identifier is declared as datatype constructor and used as an exception\n\n"))
    ((LON) (insert "Additional info: Occurs when an identifier is declared as a value variable and used as a datatype constructor\n\n"))
    ((DCE) (insert "Additional info: Occurs when an identifier is declared as an exception and used as a datatype constructor\n\n"))
    ((UNG) (insert "Additional info: Occurs when SML does not allow generalisation of type variables at certain value declarations\n\n"))
    ((INC) (insert "Additional info: Occurs when there is a free type variable in a datatype\nor type declaration\n\n"))
    ((APP) (insert "Additional info: Occurs when an identifier is both applied and not applied to\nan argument inside a pattern\n\n"))
    ((CAN) (insert "Additional info: Occurs when a constructor is non applied in a pattern and defined to take an argument\n\n"))
    ((CNA) (insert "Additional info: Occurs when a constructor is applied in a pattern and defined to take no argument\n\n"))
    ((FUN) (insert "Additional info: Occurs when a function is declared with two differing names\n\n"))
    ((ARG) (insert "Additional info: Occurs when a function is defined to take an\ninconsistent nuber of arguments\n\n"))
    ((FRE) (insert "Additional info: Occurs when there is a type variable in the top level\nenviroment that is not bound to anything (free)\n\n"))
    ((LAS) (insert "Additional info: Occurs when the identifier on the left of an \"as\" is not a\nvalue variable\n\n"))
    ((FNE) (insert "Additional info: Expressions with recursive value must be functions\n\n"))
    ((REA) (insert "Additional info: A real cannot occurs within a pattern\n\n"))
    ((IDE) (insert "Additional info: Free identifier\n\n"))
    )
  )

(defun skalpel-format-slice-info (slice assumptions kind)
  "Formats the details of the error for displaying to the user"

  ;; insert program slice info the *skalpel-slice-info* buffer
  (with-current-buffer "*skalpel-slice-info*"
   (end-of-buffer)
    (let ((p1 (point)))
      (insert (concat "Error (click to toggle associated info): " (car (cdr kind)) "\n\n"))

      (let ((p2 (point)))

	(add-text-properties p1 p2 '(mouse-face highlight help-echo "mouse-1: toggle showing slice information"))

	(skalpel-print-ek-desc kind)
	(insert "Slice:\n")
	(insert slice)
	(insert "\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

	;; code to bind mouse-1 to show/hide the information associated with each error
	;; we need to use eval here because we care about p1, p2 and (point) *now*, not at click time
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-1]
	    `(lambda ()
	       (interactive)
	       ;; the key lookup and execute mechanism does this:
	       ;;(set-buffer "*skalpel-slice-info*")
	       (put-text-property ,p2 ,(point)
				  'invisible
				  (not (invisible-p ,p2)))))
	  (put-text-property p1 p2 'keymap map))

	(setq skalpel-slice-info-slices (cons (cons id (cons p1 (point))) skalpel-slice-info-slices)))))

  ;; Report the appropriate mesage for the number of context dependencies in the error
  ;; 0 context dependencies
  (if (equal '0 (safe-length assumptions))
      (cons (concat "Error: " (car (cdr kind))) (cons (car kind) nil))

    ;; 1 context dependency
    (if (equal '1 (safe-length assumptions))
	(cons (concat "Error: " (car (cdr kind)) "\n\n"
		      "Context Dependency: " (prin1-to-string (car assumptions)) " is neither a datatype nor an exception constructor")
	      (cons (car kind) nil))
      ;; 2 or more context dependencies
      (let ((dependencies ""))
  	(while (> (safe-length assumptions) 0)
  	  (setq dependencies (concat dependencies (prin1-to-string (pop assumptions)) ", ")))
	(cons (concat "Error: " (car (cdr kind)) "\n\n"
		      "Context Dependencies: " (substring dependencies 0 (- (length dependencies) 2)) " are neither datatype\nnor exception constructors")
	      (cons (car kind) nil)))))
  )

(defun skalpel-find-info (info)
  "Calculates the extra details of the error messages that are used for the verbose error messages"
  ;; Determine the different details of the error
  (let ((kind (car info))
	(box  (car (cdr info)))
	(legendItems nil))
    ;; If the slice contains a box, add box to the list of items that need to be displayed in the legend
    (when (equal "BOX" box)
      (push 5 legendItems))
    ;; Add the extra infomation depending on the kind of error - retrieve the approprite items that need to be displayed in the legend
    (case kind
      ((CIR) (push 4 legendItems) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((OVE) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((ARI) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((TYP) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat (skalpel-get-legend legendItems)))
      ((REC) (push 4 legendItems) (push 2 legendItems) (push 1 legendItems) (push 0 legendItems) (concat (skalpel-get-legend legendItems)))
      ((UNM) (push 4 legendItems) (push 2 legendItems) (push 1 legendItems) (push 0 legendItems) (concat (skalpel-get-legend legendItems)))
      ((MSI) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat (skalpel-get-legend legendItems)))
      ((MST) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat (skalpel-get-legend legendItems)))
      ((DTC) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat (skalpel-get-legend legendItems)))
      ((PAR) (push 3 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((MUL) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((VAL) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((EXV) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((EXD) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((LON) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((DCE) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((UNG) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((INC) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((APP) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((CAN) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((CNA) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((FUN) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((ARG) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((FRE) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((LAS) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((FNE) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((REA) (push 0 legendItems) (concat  (skalpel-get-legend legendItems)))
      ((IDE) (push 3 legendItems) (concat  (skalpel-get-legend legendItems)))
      )
    )
  )

(defun skalpel-get-legend (items)
  "Produces the legend that contains the items 'items' (a list that is taken in as a paramater)"
  (let ((legend "\n\nLEGEND:\n"))
    ;; For each item needed, retrieve the appropriate inforamtion
    (while items
      (setq legend (concat legend (skalpel-get-legend-info (car items))))
      (setq items (cdr items)))
    ;; Return the legend
    legend))

(defun skalpel-get-legend-info (item)
  "Returns the approprite legend item for 'item' (the paramater of the function)"
  (let (
	(red    "RED")
	(grey   "GREY")
	(blue   "BLUE")
	(green  "GREEN")
	(purple "PURPLE")
	(yellow "YELLOW")
	(box    "BOX")
	(indent "\n\t   "))
    ;; Add the appropriate details for 'item' (the paramater of the function)
    (case item
      ((0) (concat (propertize red 'face 'skalpel-standard-error-focus) ": Indicates that the highlighted text contributes to the error.\n"))
      ((1) (concat (propertize grey   'face 'skalpel-end-point-two-focus) "/" (propertize blue 'face 'skalpel-end-point-one-focus) ": Indicates that the highlighted text is an end point of a type\nconstructor clash, an arity clash or a record clash.\n"))
      ((2) (concat (propertize green  'face 'skalpel-merged-regions-focus) ": Indicates the endpoint of a record clash. The highlighted text appears\nin both clashing records.\n" ))
      ((3) (concat (propertize yellow 'face 'skalpel-parsing-error-focus) ": Indicates that either;" indent "1. The file contains a syntax error," indent "2. The file contains a feature of SML not yet handled by the type" indent "   error slicer," indent "3. The highlighted file is not a valid \".sml\" or \".tes\" file, or" indent "4. The highlighted file does not exit.\n"))
      ((4) (concat (propertize purple 'face 'skalpel-further-explanations-focus) ": Indicates that the highlighted code provides information about the\nstatus of an identifier or that the code is expansive.\n"))
      ((5) (concat (propertize box    'face '(:box (:line-width 2 :color t :style nil)))": Can indicate:\n\t1: The application of a function to an argument (the contents of the" indent "box) takes part in an error. It is usually convenient to surround" indent "the argument of a function when the application participates in the" indent "error,or\n\t2: The contents of the box are the unique argument of a type name to" indent "make explicit that its arity is 1 (that is, has one argument). This" indent "is because there is no section of code to highlight, making explicit" indent "when its arity is 1 .\n" ))
      )))

(defun skalpel-get-id-slice (slice)
  "Returns the identifier of the slice"
  ;; The slice is a list of overlays, and these overlays should all have the same identifier
  (let ((id nil))
    (if (car slice)
	(setq id (overlay-get (car slice) 'skalpel-id))
      id)
    )
  )

(defun skalpel-highlight-next-slice (slice)
  "Highlight the slice given in argument"
  ;; Set the next slice to be in focus
  (skalpel-focus-slice slice)
  (skalpel-maybe-display-slice-as-much-as-possible slice)
  ;; Set the parts of the slice that have not been viewed to be the whole slice
  (setq skalpel-current-slice-not-viewed slice)
  ;; Focus on the begining of the slice
  (if skalpel-auto-display-entire-slice
       (skalpel-focus-next-part-of-slice slice))
  ;;(2010-06-14)DEBUG:(print (format "[CURRENTLY HIGHLIGHTING %d]" (skalpel-get-id-slice slice)))
  ;; Assign the identifier of the newly highlighted slice to the variable "skalpel-current-highlighted-slice"
  (setq skalpel-current-highlighted-slice (skalpel-get-id-slice slice))
  )

(defun skalpel-next-slice ()
  "Highlights the next slice in the list"
  (interactive)
  ;; If the current slicer pointer is non nil
  (if skalpel-current-slice-pointer
      ;; If there is a next slice
      (if (car skalpel-current-slice-pointer)
	  (progn
	    ;; Add the first item of the current slice pointer to the end of the list
	    (setq skalpel-current-slice-pointer (append skalpel-current-slice-pointer (cons (car skalpel-current-slice-pointer) nil)))
	    ;; Set current slice pointer to be the tail of itself
	    (setq skalpel-current-slice-pointer (cdr skalpel-current-slice-pointer))
	    (skalpel-highlight-next-slice (car skalpel-current-slice-pointer))
	    )
	;; The current slice pointer is nil, set it to point to all of the slices
	;; and unfocus all of the slices.
	(skalpel-unfocus-all-slices)
	(setq skalpel-current-slice-pointer (append skalpel-current-slice-pointer (cons (car skalpel-current-slice-pointer) nil)))
	(setq skalpel-current-slice-pointer (cdr skalpel-current-slice-pointer))
	)
    ;; The current slice pointer is nil, set it to point to all of the slices
    ;; and unfocus all of the slices.
    (setq skalpel-current-slice-pointer (butlast skalpel-slices))
    (skalpel-unfocus-all-slices)))

(defun skalpel-next-slice-by-id (id)
  "Highlights the next slice in the list that has the identifier id"
  (let ((done  nil)
	(firstid nil))
    (if skalpel-current-slice-pointer
	(progn
	  (if (car skalpel-current-slice-pointer)
	      (setq firstid (skalpel-get-id-slice (car skalpel-current-slice-pointer)))
	    (setq done t)
	    )
	  (while (not done)
	    (if (car skalpel-current-slice-pointer)
		(progn
		  (setq skalpel-current-slice-pointer (append skalpel-current-slice-pointer (cons (car skalpel-current-slice-pointer) nil)))
		  (setq skalpel-current-slice-pointer (cdr skalpel-current-slice-pointer))
		  ;;(skalpel-trace (format "ID: %d - %d - %d" (skalpel-get-id-slice (car skalpel-current-slice-pointer)) id firstid))
		  (if (equal firstid (skalpel-get-id-slice (car skalpel-current-slice-pointer)))
		      ;; We didn't find the error id
		      (setq done t)
		    ) ;; END IF
		  (if (equal id (skalpel-get-id-slice (car skalpel-current-slice-pointer)))
		      (progn
			(skalpel-highlight-next-slice (car skalpel-current-slice-pointer))
			(setq done t)
			)
		    ) ;; END IF
		  ) ;; END PROGN
	      (setq done t)
	      ) ;; END IF
	    ) ;; END WHILE
	  ) ;; END PROGN
      ) ;; END IF
    ) ;; END LET
  ) ;; END FUN

(defun skalpel-prev-slice ()
  "Highlights the previous slice in the list"
  (interactive)
  ;; If the current slicer pointer is non nil
  (let ((last (last skalpel-current-slice-pointer)))
    (if last
	;; If there is a previous slice
	(if (car last)
	    (progn
	      ;; Set the previous slice to be in focus
	      (skalpel-focus-slice (car last))
	      (skalpel-maybe-display-slice-as-much-as-possible (car last))
	      ;; Set the parts of the slice that have not been viewed to be the whole slice
	      (setq skalpel-current-slice-not-viewed (car last))
	      ;; Focus on the begining of the slice
	      ;;	      (skalpel-next-part-of-slice)
	      (skalpel-focus-next-part-of-slice (car skalpel-current-slice-pointer))
	      ;; Remove the last slice from the list that current slice pointer points to
	      (setq skalpel-current-slice-pointer (butlast skalpel-current-slice-pointer))
	      ;; Add the last slice to the begining of the list that current slice pointer points to
	      (push (car last) skalpel-current-slice-pointer))
	  ;; The current slice pointer is nil, set it to point to all of the slices
	  ;; and unfocus all of the slices.
	  (skalpel-unfocus-all-slices)
	  (setq skalpel-current-slice-pointer (butlast skalpel-current-slice-pointer))
	  (push (car last) skalpel-current-slice-pointer))
      ;; The current slice pointer is nil, set it to point to all of the slices
      ;; and unfocus all of the slices.
      (setq skalpel-current-slice-pointer (butlast skalpel-slices))
      (skalpel-unfocus-all-slices))))

(defun skalpel-overlay-in-view (overlay)
  "Tests whether or not the overlay is in view"
  (let ((ov-start (overlay-start overlay))
	(ov-end (overlay-end overlay)))
    ;; If the buffer of the overlay is not currently in view ...
    (if (not (get-buffer-window (overlay-buffer overlay)))
	;; ... then the overlay is not in view, so return nil ...
	nil
      ;; ... otherwise, if the whole of the overlay is in view ...
      (if (and (pos-visible-in-window-p ov-start) (pos-visible-in-window-p ov-end))
	  ;; ... return true ...
	  t
	;; ... otherwise return nil
	nil
	))))

(defun skalpel-next-part-of-slice ()
  "Displays the next part of the slice that is not currently in view"
  (interactive)
  (let ((list skalpel-current-slice-not-viewed))
    (setq skalpel-current-slice-not-viewed nil)
    ;; If all parts of the current slice have been viewed
    ;; then set the list that contains the overlays which
    ;; have yet to be viewed to contain all of the overlays
    ;; in the current slice
    (when (not list)
      (setq skalpel-current-slice-not-viewed (car skalpel-current-slice-pointer))
      (skalpel-next-part-of-slice)
      )
    ;; Foucs the next part of the current slice which is not currently visible
    (skalpel-focus-next-part-of-slice list)
    ;; Update the list of the overlays in the current slice which have yet to be viewed
    (while list
      (when (not (skalpel-overlay-in-view (car list)))
	(setq skalpel-current-slice-not-viewed (append skalpel-current-slice-not-viewed (cons (car list) nil)))
	)
      (setq list (cdr list))
      )
    )
  )

(defun skalpel-focus-next-part-of-slice (list)
  "Focuses on the next part of the current slice which is not currently visible"
  (let ((overlay (car list)))
    (when overlay
      ;; When the next overlay has no buffer (i.e. the buffer has been killed)...
      (if (not (overlay-buffer overlay))
	  ;; tidy up the list of slices so that it does not contain overlays which
	  ;; exist in buffers which do not exist.
	  ;; *** TO FIX: After tidying, if this slice still exists,
	  ;; display part of it.  If this slice has been tidied away,
	  ;; is it really okay to just silently do nothing?
	  (skalpel-tidy-slices)

	;; switch-to-buffer and/or select-window will be enough to
	;; arrange to change the buffer the next time the user is
	;; asked for input, but we may still be in an old unrelated
	;; buffer, so change now.
	(set-buffer (overlay-buffer overlay))

	;; Set the selected window to show the buffer that contains the overlay
	(let ((w (get-buffer-window (current-buffer))))
	  (if w
	      (select-window w)
	    (switch-to-buffer (current-buffer))
	    (setq w (selected-window)))

	  (goto-char (overlay-start overlay))

	  ;; just in case already displaying buffer with different window-point
	  (set-window-point w (point))

	  (save-excursion
	    (beginning-of-line 0) ;; moves to beginning of previous line
	    (set-window-start w (point))))))))


(defun skalpel-skip-past-word-if-in-one ()
  "If the point is currently “inside” an SML identifier (which means
immediately after some character of the identifier), moves to the end
of it (and past 1 whitespace character if there is one)."
  (when (and (not (bobp))
	     (save-excursion
	       (backward-char)
	       (looking-at "[A-Za-z0-9_'][A-Za-z0-9_']")))
    (skip-chars-forward "A-Za-z0-9_'")
    (if (looking-at "\\s-")
	(forward-char))))

(defconst skalpel-dec-spec-keyword-list
  '("datatype" "eqtype" "exception" "structure" "type" "val"
    "signature" "struct" "sig" "end"))

(defconst skalpel-dec-spec-keyword-regexp
  (mapconcat #'identity
	     skalpel-dec-spec-keyword-list
	     "\\|"))

(defconst skalpel-backward-to-dec-regexp
  (format "\\(%s\\)\\s-" skalpel-dec-spec-keyword-regexp))

(defun skalpel-backward-to-dec ()
  "Moves to the beginning of the first SML keyword in
skalpel-dec-spec-keyword-list that begins before point and is followed
by a whitespace character."
  (interactive)
  (skalpel-skip-past-word-if-in-one)
  (while (and (re-search-backward
	       "[A-Za-z0-9_']"
	       nil
	       'move)
	      (skip-chars-backward "[A-Za-z0-9_']")
	      (not (looking-at skalpel-backward-to-dec-regexp)))))

(defconst skalpel-forward-to-dec-regexp
  (format "[^A-Za-z0-9_']\\(%s\\)\\s-" skalpel-dec-spec-keyword-regexp))

(defun skalpel-forward-to-dec ()
  "Moves to the beginning of the first SML keyword in
skalpel-dec-spec-keyword-list that begins at least one character after
point and is followed by a whitespace character."
  (interactive)
  (if (re-search-forward skalpel-forward-to-dec-regexp nil 'move)
      (goto-char (1+ (match-beginning 0)))))

;; *** Maybe instead use the new aput function from assoc.el?
(defun skalpel-alist-put (sym key val)
  "(skalpel-alist-put SYMBOL KEY VALUE) arranges that a later call to
(skalpel-alist-get SYMBOL KEY) will return VALUE (provided there is no
							   intervening call to skalpel-alist-put with the same SYMBOL and KEY and
							   a different VALUE."
  (let ((item (assoc key (symbol-value sym))))
    (if item
	(setcdr item val)
      (push (cons key val) (symbol-value sym)))))

;; *** Maybe instead use the new aget function from assoc.el?
(defun skalpel-alist-get (sym key)
  "(skalpel-alist-get SYMBOL KEY) will return nil if
there has been no previous corresponding call of (skalpel-alist-put
SYMBOL KEY VALUE)."
  (cdr (assoc key (symbol-value sym))))

(defun skalpel-maybe-display-slice-as-much-as-possible (ovs)
  (if skalpel-auto-display-entire-slice
      (skalpel-display-slice-as-much-as-possible ovs)))

;; *** TODO: Bind this to a key!
(defun skalpel-display-slice-as-much-as-possible (ovs)
  "Function to fit as much of the current slice on the screen as possible"
  (interactive (list (car skalpel-current-slice-pointer)))
  (let (bufs earliests latests overlays)
    (dolist (ov ovs)
      (let* ((buf (overlay-buffer ov))
	     (old-earliest (skalpel-alist-get 'earliests buf))
	     ;;(old-latest (skalpel-alist-get 'latests buf))
	     )
	(pushnew buf bufs)
	(skalpel-alist-put 'overlays
			   buf
			   (adjoin ov
				   (skalpel-alist-get 'overlays buf)))
	(skalpel-alist-put 'earliests buf
			   (if old-earliest
			       (min old-earliest
				    (overlay-start ov))
			     (overlay-start ov)))
	;;(skalpel-alist-put 'latests buf
	;;                   (if old-latest
	;;                       (max old-latest
	;;                            (overlay-end ov))
	;;                     (overlay-end ov)))
	))
    (delete-other-windows)
    (let ((wins (list (selected-window)))
	  next-level-wins)
      (while (< (+ (length wins)
		   (length next-level-wins))
		(length bufs))
	(when (null wins)
	  (setq wins (nreverse next-level-wins))
	  (setq next-level-wins nil))
	(let ((new-win (split-window (car wins))))
	  (push (pop wins) next-level-wins)
	  (push new-win next-level-wins)))
      (setq wins (append (nreverse next-level-wins) wins))
      (dolist (buf bufs)
	(with-current-buffer buf
	  (let ((is-basis
		 ;; *** don't test against the string "basis.sml".  do this right!
		 ;; why don't we test: (equal skalpel-basis-file buffer-file-name)
		 (equal skalpel-basis-file buffer-file-name))
		(win (pop wins)))
	    (set-window-buffer win buf)
	    (set-window-start
	     win
	     (save-excursion
	       (goto-char (skalpel-alist-get 'earliests buf))
	       (cond (is-basis
		      (forward-char 1)
		      (skalpel-backward-to-dec))
		     (t
		      (beginning-of-line)))
	       (point)))
	    (when is-basis
	      ;; *** instead of disabling undo, this should be done
	      ;; with overlays instead of text properties.
	      ;; *** Setting read only and disabling undo should be
	      ;; done elsewhere.
	      ;; *** read-only should be adjusted with let so it is
	      ;; properly restored in case of abort.
	      (setq buffer-read-only nil)
	      (buffer-disable-undo)
	      (put-text-property (point-min) (point-max) 'display nil)
	      (goto-char (point-min))
	      (let ((sorted-ovs
		     (sort (skalpel-alist-get 'overlays buf)
			   (lambda (o1 o2)
			     (< (overlay-start o1)
				(overlay-start o2))))))
		(while sorted-ovs
		  (let ((ov (pop sorted-ovs))
			(pos (save-excursion (beginning-of-line) (point))))
		    (goto-char (1+ (overlay-start ov)))
		    (skalpel-backward-to-dec)
		    (put-text-property pos (save-excursion (beginning-of-line) (point)) 'display "\n")
		    (skalpel-forward-to-dec)
		    (while (and sorted-ovs
				(< (overlay-start (car sorted-ovs))
				   (point)))
		      (pop sorted-ovs)))))
	      (put-text-property (point) (point-max) 'display "\n")
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil)
	      ;; (goto-char (1+ (skalpel-alist-get 'earliests buf)))
	      ;; ;;(put-text-property (point) (1+ (point)) 'display "•")
	      ;; (skalpel-backward-to-dec)
	      ;; ;;(put-text-property (point) (1+ (point)) 'display "‣")
	      ;; (put-text-property (point-min) (point) 'invisible t)
	      ;; (goto-char (skalpel-alist-get 'latests buf))
	      ;; ;;(put-text-property (1- (point)) (point) 'display "◦")
	      ;; (skalpel-forward-to-dec)
	      ;; ;;(put-text-property (1- (point)) (point) 'display "⁃")
	      ;; (put-text-property (point) (point-max) 'invisible t)
	      )))))))

(defun skalpel-adjust-slice-focus (ovs focus)
  "Adjust the focus of the slice that is made up of the overlays 'ovs'"
  (mapc
   (lambda (ov) (skalpel-set-overlay-properties ov focus))
   ovs))

(defun skalpel-focus-slice (slice)
  "Sets the slice 'slice' to be in focus"
  (mapc
   (lambda (sl)
     (skalpel-adjust-slice-focus
      sl
      (or (null slice)
	  (eq slice sl))))
   skalpel-slices))

;; This function is not called at the present time
(defun skalpel-print-error-message-buffer ()
  "Prints the error messages in the message buffer"
  (let ((error-message (overlay-get (caar skalpel-current-slice-pointer) 'skalpel-info))
	(error-message-extra-details (overlay-get (caar skalpel-current-slice-pointer) 'skalpel-more-info)))
    ;; Remove all of the text properties from the error messages
    (set-text-properties 0 (length error-message) nil error-message)
    (set-text-properties 0 (length error-message-extra-details) nil error-message-extra-details)
    ;; Print error message
    (print "****\tError Message (start)\t****")
    ;; If the user has the verbose error messgae option selected ...
    (if skalpel-verbose-error-messages
	;; ... then print the verbose error messages ...
	(print (concat error-message error-message-extra-details))
      ;; ... otherwise print the shortened error message
      (print error-message))
    (print "****\tError Message (end)\t****")
    )
  )

(defun skalpel-unfocus-all-slices ()
  "Sets all of the slices to be out of focus"
  (mapc
   (lambda (sl)
     (skalpel-adjust-slice-focus
      sl
      nil))
   skalpel-slices)
  )

(defun skalpel-forget-all-slices ()
  "Removes all of the details about all of the slices"
  (interactive)
  (skalpel-trace "FORGETTING ENTER")
  ;; stops the skalpel process if it exists
  (skalpel-stop-skalpel-process)
  ;; deletes the temporary directory
  (skalpel-delete-temporary-directory)
  ;; removes the highlightings
  (while skalpel-overlays
    (delete-overlay (pop skalpel-overlays)))
  (setq skalpel-slices nil)
  (setq skalpel-current-slice-pointer nil)
  (setq skalpel-current-highlighted-slice nil)
  )

(defun skalpel-forget-all-slices-file ()
  "Removes all of the details about all of the slices in the current file"
  (interactive)
  ;; Set pointer to be an exact copy of skalpel-overlays
  (let ((pointer skalpel-overlays)
	(buffer (current-buffer)))
    ;; Set skalpel-overlays to be nil
    (setq skalpel-overlays '())
    ;; While we have not looked at all of the overlays
    (while (car pointer)
      ;; If the overlay we are looking at is in the current buffer delete it...
      (if (equal (overlay-buffer (car pointer)) buffer)
	  (delete-overlay (car pointer))
	;; ...otherwise add it back to skalpel-overlays
	(push (car pointer) skalpel-overlays))
      ;; Increment the pointer to look at the next slice
      (setq pointer (cdr pointer)))
    ;; If all of the slices we in this buffer, then...
    (if (equal 0 (safe-length skalpel-overlays))
	(progn
	  ;; Set the pointer to the current slice to nil
	  (setq skalpel-current-slice-pointer nil)
	  ;; Set the list of Slices to nil
	  (setq skalpel-slices nil)
	  ;; Set the identifier of the currently highlighted slice to nil
	  (setq skalpel-current-highlighted-slice nil)
	  )
      )
    ;; Tidy up the slcies
    (skalpel-tidy-slices)))

(defun skalpel-forget-slice-by-id (id)
 "Removes the slice with id 'id'. Used to remove slices when they are no longer needed
 i.e. when Skalpel merges 2 or more slices"

  ;; remove the slice from the list of slice information in *skalpel-slice-info* buffer
  (defvar looplist)
  (setq looplist skalpel-slice-info-slices)

  (with-current-buffer "*skalpel-slice-info*"
  (catch 'break (while (not (equal looplist nil))
  (if (equal (car (car looplist)) id)
      (progn (put-text-property (car (cdr (car looplist))) (cdr (cdr (car looplist))) 'invisible t) (throw 'break nil))
      (setq looplist (cdr looplist))))))

  ;; Set pointer to be an exact copy of skalpel-overlays
  (let ((pointer skalpel-overlays))
    ;; Set skalpel-overlays to be nil
    (setq skalpel-overlays '())
    ;; While we have not looked at all of the overlays
    (while (car pointer)
      ;; If the current overlay is in the slice with id 'id' delete it...
      (if (equal id (overlay-get (car pointer) 'skalpel-id))
	  (delete-overlay (car pointer))
	;; ...otherwise add it back to the list of overlays
	(push (car pointer) skalpel-overlays))
      ;; Look at the next overlay in the list
      (setq pointer (cdr pointer)))
    ;; If all of the overlays were in the slice with id 'id'
    (if (equal 0 (safe-length skalpel-overlays))
	(progn
	  ;; Set the pointer to the current slice to nil
	  (setq skalpel-current-slice-pointer nil)
	  ;; Set the list of Slices to nil
	  (setq skalpel-slices nil)))
    ;; Tidy up the slcies
    (skalpel-tidy-slices)))

;; *** TO FIX in skalpel-tidy-slices:
;;
;; 1. This reverses the order of the slices, thereby confusing
;;     the user.
;;
;; 2. Why is this putting a nil entry in the list?  It can gets
;;    confused if nil exists in the list in the middle.  (Can this
;;    happen?)
;;
;; 3. This does not check that every overlay in each slice still
;;    exists.  It only checks the buffer of the first overlay in each
;;    slice.
;;
(defun skalpel-tidy-slices ()
  "Ensures that the list of slices is tidy (there are no overlays that have been deleted)"
  ;; Set pointer to be an exact copy of skalpel-slices
  (let ((pointer skalpel-slices))
    ;; Set skalpel-slices to be nil
    (setq skalpel-slices nil)
    ;; Add 'nil' to the beginning of the list
    (push nil skalpel-slices)
    ;; While we have not looked at all of the slices
    (while (car pointer)
      ;; When the current slice has not been deleted (exists in a buffer) add it back to skalpel-slices
      (when (not (equal (overlay-buffer (caar pointer)) nil))
	(push (car pointer) skalpel-slices))
      ;; Increment the pointer to look at the next slice
      (setq pointer (cdr pointer))
      ))
  ;; Set the current-slice-pointer to point to the list of slices
  (setq skalpel-current-slice-pointer (butlast skalpel-slices)))

(defun skalpel-trace (msg &rest data)
  "Writes a message and some data (optional) so that useful information can be displayed to the user"
  (apply #'message (concat "(SKALPEL) " msg) data)
  (if (fboundp 'redisplay)
      (redisplay t)))

(defun skalpel-set-timelimit (time)
  "Changes the time that Skalpel is run for"
  (interactive "nnew time limit:")
  (setq skalpel-timelimit time))

(defun skalpel-set-basis-option (option)
  "Changes the basis option (0/1/2 as explained above)"
  (interactive "nnew basis option:")
  (setq skalpel-basis-option option))

(defun skalpel-toggle-verbose ()
  "Toggle whether or not the verbose error messages are shown"
  (interactive)
  (setq skalpel-verbose-error-messages (not skalpel-verbose-error-messages)))

(defun skalpel-toggle-show-slice ()
  "Toggle whether or not slice information is automatically shown"
  (interactive)
  (setq skalpel-show-slice (not skalpel-show-slice)))

(defun skalpel-toggle-arrange-windows ()
  "Toggles whether windows are arranged to show information from various files"
  (interactive)
  (setq skalpel-auto-display-entire-slice (not skalpel-auto-display-entire-slice)))

(defun skalpel-toggle-show-basis ()
  "Toggle whether or not slice information is automatically shown"
  (interactive)
  (setq skalpel-show-basis (not skalpel-show-basis)))

(defun skalpel-show-help ()
  "Loads and displays the help file for Skalpel"
  (interactive)
  (let* (;; *** not needed to calculate cur-window
	 (cur-window (selected-window))
	 (help-window (split-window
		       ;; *** not needed to use cur-window (nil is equivalent here)
		       cur-window nil)))
    (select-window help-window)
    ;; *** probably a bad idea to split the window before running
    ;; view-file, because when viewing is done the split will not be
    ;; undone.
    (view-file (expand-file-name "SKALPEL-HELP" skalpel-emacs-directory))
    )
  )

(defun skalpel-set-compiler-smlnj ()
  "Toggle SML/NJ as preferred compiler"
  (interactive)
  (setq skalpel-user-compiler-smlnj (not skalpel-user-compiler-smlnj))
  (setq skalpel-user-compiler-mlton nil)
  (setq skalpel-user-compiler-polyml nil)
  )

(defun skalpel-set-compiler-mlton ()
  "Toggle MLton as preferred compiler"
  (interactive)
  (setq skalpel-user-compiler-mlton (not skalpel-user-compiler-mlton))
  (setq skalpel-user-compiler-smlnj nil)
  (setq skalpel-user-compiler-polyml nil)
  )

(defun skalpel-set-compiler-polyml ()
  "Toggle PolyML as preferred compiler"
  (interactive)
  (setq skalpel-user-compiler-polyml (not skalpel-user-compiler-polyml))
  (setq skalpel-user-compiler-mlton nil)
  (setq skalpel-user-compiler-smlnj nil)
  )

(defun skalpel-run-compiler-after-slicer (file)
  "Runs SML/NJ only if Skalpel is not running and if Skalpel hasn't found any error"
  (let ((proc (get-process "skalpel")))
    (if (equal nil proc)
	(if (< skalpel-item-count 1)
	    (progn
	      (if skalpel-user-compiler-smlnj
		  (progn
		    (split-window)
		    (shell)
		    (process-send-string "shell" "sml\n")
		    (process-send-string "shell" (format "use \"%s\";\n" file))
		    )
		)
	      (if skalpel-user-compiler-mlton
		  (progn
		    (split-window)
		    (shell)
		    (process-send-string "shell" (format "mlton" file "-verbose 1"))
		    (process-send-string "shell" "\n")
		    )
		)
	      (if skalpel-user-compiler-polyml
		  (progn
		    (split-window)
		    (shell)
		    (process-send-string "shell" "poly\n")
		    (process-send-string "shell" (format "PolyML.use \"%s\"\n" file))
		    )
		)
	      )
	  )
      (run-at-time 1 nil 'run-compiler-after-slicer file)
      )
    )
  )

(defun skalpel-run-slicer-then-compiler ()
  "Runs TES then SML/NJ if Skalpel does not find any error"
  (interactive)
  (skalpel-run-slicer-exec)
  (run-compiler-after-slicer (buffer-file-name))
  )

(defun skalpel-submit-bug ()
  "Opens the user's default web browser to our Bugzilla bug tracking system."
  (interactive)
  (progn
    (browse-url "http://www.macs.hw.ac.uk/ultra/compositional-analysis/type-error-slicing/bugzilla-4.0/index.cgi")
    ))
