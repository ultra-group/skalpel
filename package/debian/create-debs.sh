###############################################################
###############################################################
##
## Copyright 2009, 2010 Steven Shiells
## Copyright 2011 John Pirie
## Copyright 2010, 2011 Heriot-Watt University
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
## Description: This is a shell script to run Skalpel, type 
##              error slicer for SML.
##
################################################################
################################################################


#############################################################################################
# need to look out for version numbers, at the moment they are hard
# coded into the script
#############################################################################################

#!/bin/bash

currentDir=`pwd`

make skalpel
mkdir temp
mv skalpel-0.7 temp/
cd temp
mv skalpel-0.7/debian skalpel-0.7/build-stamp skalpel-0.7/configure-stamp ./
tar -cvzf skalpel-0.7.tar.gz skalpel-0.7/
cd skalpel-0.7
dh_make -c GPL3 -e jp95@hw.ac.uk -f ../skalpel-0.7.tar.gz
rm -rf debian
mv ../debian/ ../build-stamp ../configure-stamp ./
dpkg-buildpackage -rfakeroot

cd $currentDir
make emacs
mkdir tempemacs
mv skalpel-emacs-0.7 tempemacs/
cd tempemacs
mv skalpel-emacs-0.7/debian skalpel-emacs-0.7/build-stamp skalpel-emacs-0.7/configure-stamp ./
tar -cvzf skalpel-emacs-0.7.tar.gz skalpel-emacs-0.7/
cd skalpel-emacs-0.7
dh_make -c GPL3 -e jp95@hw.ac.uk -f ../skalpel-emacs-0.7.tar.gz
rm -rf debian
mv ../debian/ ../build-stamp ../configure-stamp ./
dpkg-buildpackage -rfakeroot
