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
## Description: This is a script which when executed by the user,
##              will run tests on various elements of the skalpel
##              project and output the results to the ./test-results
##              folder
##
###############################################################
###############################################################

#!/bin/bash

# the current date in the format YEAR-MONTH-DAY-HOUR-MINUTE
date=`date '+%Y-%m-%d'`
yesterday=`date --date="yesterday" '+%Y-%m-%d'`

repoDir='/u1/pg/jp95/repos/skalpel'

# directory for the skalpel binary
skalpelBin="$repoDir/analysis-engines/standard-ml/bin/skalpel"

# directory where the analysis engine test files are stored
analysisTestDir="$repoDir/testing/analysis-engine-tests/standard-ml/"

# location of the basis file
basisFile="$repoDir/lib/basis.sml"

# director for the test framework
testFrameworkDir="$repoDir/testing"

# names of the files which are output
analysisTestFilename="skalpel-engine-test-$date"
deadLinksTestFilename="skalpel-dead-links-$date"
analysisTestFilenameYesterday="skalpel-engine-test-$yesterday"
deadLinksTestFilenameYesterday="skalpel-dead-links-$yesterday"

# createt the output directory
outputDir="$repoDir/testing/test-results/$date"
yesterdayOutputDir="$repoDir/testing/test-results/$yesterday"
mkdir -p $outputDir

# navigate to the skalpel repository, and pull the latest changes
(cd $repoDir; git pull http master)

# rebuild the skalpel binary
compilationLog=`mktemp`
(cd "$repoDir/analysis-engines/standard-ml"; make mlton-bin-gen > $compilationLog)

# run the analysis engine tests
$skalpelBin -b 2 $basisFile -c $analysisTestDir > $outputDir/$analysisTestFilename 2> $outputDir/$analysisTestFilename-errors

# if there weren't any errors that appeared in the output, remove the errors file
analysisErrors=`cat $outputDir/$analysisTestFilename-errors`
if [ ! -n "$analysisErrors" ]
then
    rm "$outputDir/$analysisTestFilename-errors"
fi

deadLinksErrors=`cat $outputDir/$deadLinksTestFilename-errors`
if [ ! -n "$deadLinksErrors" ]
then
    rm "$outputDir/$deadLinksTestFilename-errors"
fi


# check for any dead links in the webdemo
# NOTE: We can't currently do this here because we are not on the webserver,
#       therefore for the moment this is done via a web server cron job
# $testFrameworkDir/scripts/check-webdemo-links.sh > $outputDir/$deadLinksTestFilename 2> $outputDir/$deadLinksTestFilename-errors

# create a new temporary file to put the e-mail into
mailFile=`mktemp`

# construct the e-mail text
echo -e "This is an automated message sent from the Skalpel test framework.\n" > $mailFile

echo -e "This message describes the daily Skalpel test results for date\n\
$date. In each diff, the left hand side (<) represents the results\n\
from yesterday and the results on the right hand side (>) are the\n\
results from today. The compilation log is given first, then the\n\
diffs, then the full test result logs are given at the end of this\n\
message.\n" >> $mailFile


echo -e "The most recent commit message of the skalpel repository is directly below.\n" >> $mailFile

(cd $repoDir; git log | head -n 5 >> $mailFile)

echo -e "******************************\n        Compilation Log         \n******************************" >> $mailFile

cat $compilationLog >> $mailFile

echo -e "\n\n******************************\n  Analysis Engine Tests Diff     \n******************************" >> $mailFile

diff $yesterdayOutputDir/$analysisTestFilenameYesterday $outputDir/$analysisTestFilename >> $mailFile

echo -e "\n\n******************************\n  Website Broken Links Diff   \n******************************" >> $mailFile

diff $yesterdayOutputDir/$deadLinksTestFilenameYesterday $outputDir/$deadLinksTestFilename >> $mailFile

echo -e "\n\n******************************\n  Analysis Engine Tests Log     \n******************************" >> $mailFile

cat $outputDir/$analysisTestFilename >> $mailFile

echo -e "\n\n******************************\n  Website Broken Links Log   \n******************************" >> $mailFile

cat $outputDir/$deadLinksTestFilename >> $mailFile

# apparently you can't send mail to non-HW adresses with this. There's probably a way around that though.
cat $mailFile | mail -s "Skalpel daily test $date" jp95@macs.hw.ac.uk jbw@macs.hw.ac.uk

rm $mailFile
rm $compilationLog
