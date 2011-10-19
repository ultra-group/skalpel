;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2009, 2010 Steven Shiells
;; Copyright 2009, 2010, 2011 Heriot-Watt University
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
;; * Author(s): Steven Shiells, Scott Fotheringham, John Pirie
;; * Date: 16 September 2009
;; * Description:
;;     The 'Errors' menu for Skalpel from which we can
;;     for example run Skalpel on a file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skalpel-enable-menu ()
	"Enables Skalpel's Errors menu in Emacs when called"
	(define-key-after (lookup-key global-map [menu-bar]) [skalpel-menu]
    	(cons "Skalpel" (make-sparse-keymap "Skalpel"))
    'tools)

	;; submenu, Slicing
	(define-key-after global-map [menu-bar skalpel-menu slicing]
	 	(cons "Slicing" (make-sparse-keymap "slicing")) nil)

		(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing show-help]
	 		'(menu-item "Help" skalpel-show-help
				:help "Shows the help text for Skalpel"))

		(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing verbose-errors]
	   		'(menu-item "Verbose Error Messages?" skalpel-toggle-verbose
				:button (:toggle . skalpel-verbose-error-messages)))

		(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing slice-information]
	   		'(menu-item "Automatically display slice information" skalpel-toggle-show-slice
				:button (:toggle . skalpel-show-slice)))

		(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing show-basis]
	   		'(menu-item "Show basis information for overloading errors" skalpel-toggle-show-basis
				:button (:toggle . skalpel-show-basis)))

		(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing arrange-windows]
	   		'(menu-item "Arrange windows to show  other relevant files" skalpel-toggle-arrange-windows
				:button (:toggle . skalpel-auto-display-entire-slice)))

	 	(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing forget-slices]
	   		'(menu-item "Remove Highlighting from all Files" skalpel-forget-all-slices
				:help "Removes all of the highlighting produced by Skalpel in the all files" :enable skalpel-current-slice-pointer))

	 	(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing forget-slices-file]
	  		'(menu-item "Remove Highlighting from Current File" skalpel-forget-all-slices-file
				:help "Removes all of the highlighting produced by Skalpel in the current file" :enable skalpel-current-slice-pointer))

	 	(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing next-part-slice]
	   		'(menu-item "Next Part of Slice" skalpel-next-part-of-slice
				:help "Highlights the next part of the slice" :enable skalpel-current-slice-pointer))

	 	(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing prev-slice]
	   		'(menu-item "Previous Slice" skalpel-prev-slice
				:help "Highlights the previous slice" :enable skalpel-current-slice-pointer))

	 	(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing next-slice]
	   		'(menu-item "Next Slice" skalpel-next-slice
				:help "Highlights the next slice" :enable skalpel-current-slice-pointer))


		(if skalpel-developer
	    	(progn
				(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing run-skalpel]
	  				'(menu-item "Run Skalpel" skalpel-run-slicer
		      			:help "Runs Skalpel on the current file"))
				(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing run-skalpel-exec]
	  				'(menu-item "Run Skalpel from Binary" skalpel-run-slicer-exec
		     			:help "Runs Skalpel on the current file"))
			)

			(define-key (lookup-key global-map [menu-bar]) [skalpel-menu slicing run-skalpel]
	   				'(menu-item "Run Skalpel" skalpel-run-slicer-exec
		       			:help "Runs Skalpel on the current file"))
		)

		;; submenu, user preference of Compiler
		(define-key-after global-map [menu-bar skalpel-menu compiler]
		(cons "Compiler Preference" (make-sparse-keymap "compiler")) nil)
		;; if (after loading the user's .bashrc and .bash_profile) MLton is installed, add option for MLton as default compiler

		;; jpirie: the below crashes my emacs version! Keeping just the which statement for now.
		;;(not (equal "" (shell-command-to-string "source ~/.bashrc 2>/dev/null;source ~/.bash_profile 2>/dev/null;which mlton")))
		(if (not (equal "" (shell-command-to-string "which mlton")))
			(progn

			   (define-key (lookup-key global-map [menu-bar]) [skalpel-menu compiler toggle-mlton]
			    '(menu-item "Mlton" skalpel-set-compiler-mlton
					:help "Selects Mlton as primary compiler"
					:button (:toggle . skalpel-user-compiler-mlton)))
			  (skalpel-set-compiler-mlton)
			)
		)

		;; if (after loading the user's .bashrc and .bash_profile) PolyML is installed, add option for PolyML as default compiler
		;; jpirie: the below crashes my emacs version! Keeping just the which statement for now.
		;;(if (not (equal "" (shell-command-to-string "source ~/.bashrc 2>/dev/null;source ~/.bash_profile 2>/dev/null;which poly")))
                (if (not (equal "" (shell-command-to-string "which poly")))
			(progn
				(define-key (lookup-key global-map [menu-bar]) [skalpel-menu compiler toggle-polyml]
					'(menu-item "PolyML" skalpel-set-compiler-polyml
							:help "Selects PolyML as primary compiler"
							:button (:toggle . skalpel-user-compiler-polyml)))
							(skalpel-set-compiler-polyml)
			)
		)
		;; if (after loading the user's .bashrc and .bash_profile) SML/NJ is installed, add option for SML/NJ as default compiler
		;; (if (not (equal "" (shell-command-to-string "source ~/.bashrc 2>/dev/null;source ~/.bash_profile 2>/dev/null;which sml")))

		(if (not (equal "" (shell-command-to-string "which sml")))
			(progn
				(define-key (lookup-key global-map [menu-bar]) [skalpel-menu compiler toggle-smlnj]
					'(menu-item "SML/NJ" skalpel-set-compiler-smlnj
							:help "Selects SML/NJ as primary compiler"
							:button (:toggle . skalpel-user-compiler-smlnj)))
							(skalpel-set-compiler-smlnj)
			)
		)


	;; submenu, Graphical Options
	(define-key-after global-map [menu-bar skalpel-menu graphical]
    	(cons "Graphical Options" (make-sparse-keymap "graphical")) nil)

	;;template menu item (under graphical submenu)
	;;(define-key (lookup-key global-map [menu-bar]) [skalpel-menu graphical new-menu-item-name]
  	;;	'(menu-item "New Menu Item Name" skalpel-function-to-execute
	;;		:help "What it does." :enable skalpel-function-with-data))

	;; submenu, Bugs
	(define-key-after global-map [menu-bar skalpel-menu bugs]
    	(cons "Bugs" (make-sparse-keymap "bugs")) nil)

	;;menu item to link to our bug tracker
	(define-key (lookup-key global-map [menu-bar]) [skalpel-menu bugs submit-bug]
  		'(menu-item "Submit A Bug" skalpel-submit-bug
			:help "Opens a web browser window to Skalpel's Bugzilla bug tracking system."))
)
(skalpel-enable-menu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
