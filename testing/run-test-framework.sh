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

################################################################################
#                    declare variables for use in the script
################################################################################

# the current date in the format YEAR-MONTH-DAY-HOUR-MINUTE
date=`date '+%Y-%m-%d'`

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
deadLinksTestFilename="skalpel-dead-links-$date"
analysisTestMasterFilename="skalpel-engine-test"
deadLinksTestMasterFilename="skalpel-dead-links"

# createt the output directory
testingDir="$repoDir/testing/test-results"
outputDir="$testingDir/$date"
masterDir="$repoDir/testing/master-test-files"

################################################################################
#                run various tests on areas of the Skalpel project
################################################################################

# this is created for the dead links output, this directory is removed at the end
mkdir -p $outputDir

# navigate to the skalpel repository, and pull the latest changes
# jpirie: Actually, it would be nice if the test framework started from a fresh
#         clone of the repository each time, as that's what our users would do
echo "updating the skalpel repository..."
pullLog=`mktemp`
(cd $repoDir; git pull http master &>$pullLog)

# remove the old skalpel binary so we's sure we have new results
echo "removing existing skalpel analysis engine binary..."
rm -f "$skalpelBin"

# rebuild the skalpel binary
echo "building the skalpel binary..."
compilationLog=`mktemp`
(cd "$repoDir/analysis-engines/standard-ml"; make mlton-bin-gen &> $compilationLog)

# run the analysis engine tests
echo "running analysis engine tests..."
analysisTestsLog=`mktemp`
$skalpelBin -b 2 $basisFile -c $analysisTestDir &> $analysisTestsLog

# check for any dead links in the webdemo
# NOTE: We can't currently do this here because we are not on the webserver,
#       therefore for the moment this is done via a web server cron job
# $testFrameworkDir/scripts/check-webdemo-links.sh > $outputDir/$deadLinksTestFilename 2> $outputDir/$deadLinksTestFilename-errors

# wait 5 minutes for other tests to be completed
sleep 300

################################################################################
#      create the e-mail to send out to the Skalpel project developers
################################################################################

# create a new temporary file to put the e-mail into
mailFile=`mktemp`
echo "creating mailFile at location $mailFile..."

# construct the e-mail text
echo -e "This is an automated message sent from the Skalpel test framework.\n" > $mailFile

echo -e "This message describes the daily Skalpel test results for date\n\
$date. In each diff, the left hand side (<) represents the master\n\
file results and the results on the right hand side (>) are the\n\
results from today. The compilation log is given first, then the\n\
diffs, then the full test result logs are given at the end of this\n\
message.\n" >> $mailFile


echo -e "The most recent commit message of the skalpel repository is directly below.\n" >> $mailFile

(cd $repoDir; git log | head -n 6 >> $mailFile)

echo -e "\n\n****************************************\n       Updating Repository Log         \n****************************************" >> $mailFile

cat $pullLog >> $mailFile

echo -e "\n\n****************************************\n    Analysis Engine Compilation Log         \n****************************************" >> $mailFile

cat $compilationLog >> $mailFile

echo -e "\n\n****************************************\n      Analysis Engine Version          \n****************************************" >> $mailFile

echo `$skalpelBin -v` >> $mailFile

echo -e "\n\n******************************\n  Analysis Engine Tests Diff     \n******************************" >> $mailFile

diff $masterDir/$analysisTestMasterFilename $analysisTestsLog >> $mailFile

echo -e "\n\n******************************\n  Website Broken Links Diff   \n******************************" >> $mailFile

# check that the webserver produced a log
if [ -f "$outputDir/$deadLinksTestFilename" ]
then
    diff $masterDir/$deadLinksTestMasterFilename $outputDir/$deadLinksTestFilename >> $mailFile
else
    echo "An error has been detected: The webserver has not produced a log of broken website links." >> $mailFile
fi

echo -e "\n\n******************************\n  Analysis Engine Tests Log     \n******************************" >> $mailFile

cat $analysisTestsLog >> $mailFile

echo -e "\n\n******************************\n  Website Broken Links Log   \n******************************" >> $mailFile

# check that the webserver produced a log
if [ -f "$outputDir/$deadLinksTestFilename" ]
then
    cat $outputDir/$deadLinksTestFilename >> $mailFile
else
    echo "An error has been detected: The webserver has not produced a log of broken website links." >> $mailFile
fi

# apparently you can't send mail to non-HW adresses with this. There's probably a way around that though.
cat $mailFile | mail -s "Skalpel daily test $date" jp95@macs.hw.ac.uk jbw@macs.hw.ac.uk

# remove the temporary files
rm -f $mailFile $compilationLog $analysisTestsLog
rm -rf $testingDir
