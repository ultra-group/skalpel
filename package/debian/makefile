###############################################################
###############################################################
##
## Copyright 2010 Steven Shiells
## Copyright 2011 Heriot Watt University
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
## Authors: Steven Shiells, John Pirie
## Date: December 2009
## Description: This is a shell script to run Skalpel, type 
##              error slicer for SML.
##
################################################################
################################################################

repoDir=../../
sourcesDir=${repoDir}analysis-engines/standard-ml/
libDir=${repoDir}lib/

sharedDir=../shared/

emacsDir=${repoDir}ui/EmacsUI/

commLineName=skalpel

tesname="skalpel"
version="0.8"

emacsname="skalpel-emacs"
emacsversion="0.8"

skalpeltmp="skalpeltmp"

default:
	# OPTIONS
	# make skalpel   : makes the directory required for the type error slicer package
	# make emacs : makes the directory required for the emacs extension package
	# make clean : cleans the working directory

skalpel:
	#Delete existing directory
	@rm -rf ${tesname}-${version}
	#Create a directory containing all of the necessary files
	#Get a copy of all of the necessary files
	@cp -R ${tesname}.in                            ${tesname}-${version}
	@cp ${sourcesDir}/bin/skalpel                   ${tesname}-${version}
	@cp ../../documentation/end-user/man-pages/skalpel.1              ${tesname}-${version}/debian
	@cp ${libDir}basis.sml                          ${tesname}-${version}
	@cp ${sharedDir}skalpel/*                       ${tesname}-${version}/debian
	@echo "You can find the files in \"${tesname}-${version}\""
	# ./create-debs.sh
emacs:
	curDir=`pwd`
	#Delete existing directory
	@rm -rf ${emacsname}-${emacsversion}
	#Create a directory containing all of the necessary files
	#Get a copy of all of the necessary files
	@cp -R ${emacsname}.in                        ${emacsname}-${emacsversion}
	@cp ${emacsDir}skalpel-config.el              ${emacsname}-${emacsversion}
	@cp ${emacsDir}skalpel-debug-utils.el         ${emacsname}-${emacsversion}
	@cp ${emacsDir}skalpel-main.el                ${emacsname}-${emacsversion}
	@cp ${emacsDir}skalpel-menu.el                ${emacsname}-${emacsversion}
	@cp ${emacsDir}SKALPEL-HELP                   ${emacsname}-${emacsversion}
	@cp ${sharedDir}skalpel-emacs/*                 ${emacsname}-${emacsversion}/debian
	(cd ${emacsname}-${emacsversion}; cat skalpel-main.el | sed 's/\(defvar skalpel-slicer-bin \)\"slicer\"/\1\"skalpel-bin\"/' > skalpel-main2.el; mv skalpel-main2.el skalpel-main.el)
	(cd ${emacsname}-${emacsversion}; cat skalpel-config.el | sed 's/;;\*\*//' > skalpel-config2.el; mv skalpel-config2.el skalpel-config.el)
	@makeinfo ${repoDir}documentation/user-guide/info/emacsmanual.texi
	@mv Skalpel.info                               ${emacsname}-${emacsversion}/Scalpel.info
	@rm -f emacsmanual.*
	@echo "You can find the files in \"${emacsname}-${emacsversion}\""

clean:
	@rm -rf ${tesname}-${version}
	@rm -rf ${emacsname}-${emacsversion}