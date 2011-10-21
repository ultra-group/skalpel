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

#! /usr/bin/make

clean:
	@echo "Cleaing compelte."

install:
	cp skalpel-main.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpelemacs/
	cp skalpel-menu.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpelemacs/
	cp skalpel-config.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpelemacs/
	cp skalpel-debug-utils.el ${DESTDIR}/usr/local/share/emacs/site-lisp/skalpelemacs/
	cp SKALPEL-HELP ${DESTDIR}/usr/local/share/skalpelemacs
	cp debian/skalpelemacs.1 ${DESTDIR}/usr/local/share/man/man1
	gzip -9 ${DESTDIR}/usr/local/share/man/man1/skalpelemacs.1
	cp Skalpel.info ${DESTDIR}/usr/local/share/info
	gzip -9 ${DESTDIR}/usr/local/share/info/Skalpel.info
	@echo "Installation complete."

default:
	@echo "Compilation complete."
