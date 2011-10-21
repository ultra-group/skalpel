#! /usr/bin/make

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


clean:
	@echo "Cleaning complete."

install:
	cp basis.sml $(DESTDIR)/usr/local/share/skalpel/
	cp skalpel-perl-to-bash $(DESTDIR)/usr/local/bin/
	cp skalpel $(DESTDIR)/usr/local/bin/
	cp skalpel-bin $(DESTDIR)/usr/local/bin/
	cp skalpel-legend $(DESTDIR)/usr/local/bin/
	chmod 755 debian/skalpel.1
	chmod 755 debian/skalpel-legend.1
	chmod 755 debian/skalpel-bin.1
	chmod 755 debian/skalpel-perl-to-bash.1
	gzip -9 debian/skalpel.1
	gzip -9 debian/skalpel-legend.1
	gzip -9 debian/skalpel-bin.1
	gzip -9 debian/skalpel-perl-to-bash.1
	cp debian/skalpel.1.gz ${DESTDIR}/usr/local/share/man/man1
	cp debian/skalpel-legend.1.gz ${DESTDIR}/usr/local/share/man/man1
	cp debian/skalpel-bin.1.gz ${DESTDIR}/usr/local/share/man/man1
	cp debian/skalpel-perl-to-bash.1.gz ${DESTDIR}/usr/local/share/man/man1
	@echo "Installation complete."

default:
	@echo "Compilation complete."
