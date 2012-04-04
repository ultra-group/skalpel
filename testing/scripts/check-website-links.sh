##############################################################
##############################################################
##
## Copyright 2012 Heriot-Watt University
##
## Skalpel is free software: you can redistribute it and/or modify
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
## Authors: John Pirie
## Date: April 2012
## Description: Checks for dead links in our website
##
###############################################################
###############################################################

#!/bin/bash

# the directory which we are going to check for broken links
websiteDir="/home/www/macs/ultra/skalpel/"

# a new temporary files which contains the list of files that we are going to look through
filesList=`mktemp`

echo "Website directory set to $websiteDir"

# create a list of files to search through, store that in the new file $filesList
# we don't handle any mlton url - for some reason this is listed as forbidden, or times out
find $websiteDir -name "*.html" | xargs grep "<a href" | grep -v "old-files" | grep -v "mlton" > $filesList

# put the word 'empty' in to save an extra null check in each line iteration
previousFile="empty"

# holds how many broken links have been detected
errCount=0

while read line
do
    currentFile=`echo $line | sed s/":.*"//g`

    # we take out things like the name of the file that was left in by find so that we can just get the url
    currentUrl=`echo $line | sed s/".*href=\""//g | sed s/".*\[.*href=\""//g | sed s/"\".*"//g`

    # wget outputs on stderr, so we do 2>&1 to allow for grepping with pipes
    linkBroken=`wget -t 2 --spider $currentUrl 2>&1 | grep "broken link"`

    if [ $previousFile != $currentFile ]
    then
	previousFile=$currentFile
	echo "Processing file: $currentFile"
    fi

    if [ -n "$linkBroken" ]
    then
	errCount=$(($errCount+1))
	echo -e "*** Broken Link: $currentUrl"
    fi

done < $filesList

# show the user how many broken links were detected
echo -e "\nDetected $errCount broken links"

# remove the temporary file that we used
rm $filesList