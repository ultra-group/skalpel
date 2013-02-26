###############################################################
###############################################################
##
## Copyright 2010 Steven Shiells
##
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
##
##
## Authors: Steven Shiells
## Date: December 2009
## Description: Makefile to install the software to /
##
################################################################
################################################################

#!/usr/bin/make

clean:
	@echo "Cleaing complete."

install:
	cp skalpel-main.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpel-emacs/
	cp skalpel-menu.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpel-emacs/
	cp skalpel-config.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpel-emacs/
	cp skalpel-debug-utils.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpel-emacs/
	@echo "Installation complete."

default:
	@echo "Compilation complete."
