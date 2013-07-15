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

# run by crontab on lxultra 8, entry: 0 7 * * 1-5 . $HOME/.path-init; /u1/pg/jp95/repos/skalpel/testing/run-test-framework.sh &>/u1/pg/jp95/test-framework-output

#!/bin/bash

################################################################################
#                    declare variables for use in the script
################################################################################

# the current date in the format YEAR-MONTH-DAY-HOUR-MINUTE
date=`date '+%Y-%m-%d'`

# WARNING: the contents of $repoDir are passed to rm -rf at the end of this script.
repoDir=`mktemp -d`
echo "created temp repo dir: $repoDir"

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
compMltonTestMasterFilename="skalpel-mlton-compilation"
compPolyTestMasterFilename="skalpel-polyml-compilation"

# createt the output directory
resultsDir="$HOME/repos/skalpel/testing/test-results/`date '+%Y-%m-%d'`"
masterDir="$repoDir/testing/master-test-files"

################################################################################
#                run various tests on areas of the Skalpel project
################################################################################

# clone the repository and build the skalpel binary
# WARNING: the contents of $polyRepoDir are passed to rm -rf at the end of this script.
polyRepoDir=`mktemp -d`
echo "cloning repository and building the skalpel binary (poly/ml)..."
polyCompilationLog=`mktemp`
tempBuildFile=`mktemp`
currentDir=`pwd`
(cd /tmp; git clone https://git.gitorious.org/skalpel/skalpel.git $polyRepoDir &>$polyCompilationLog)
cd $polyRepoDir/analysis-engines/standard-ml
sed s:"val prefix.*":"val prefix = \"/u1/pg/jp95/software-64/mlton-20100608/build/lib/sml\";": build-files/polyml-libraries > $tempBuildFile
mv $tempBuildFile build-files/polyml-libraries
autoconf &> $polyCompilationLog
./configure &> $polyCompilationLog
make polyml-bin &> $polyCompilationLog

# clone the repository and build the skalpel binary
echo "cloning repository and building the skalpel binary (mlton)..."
compilationLog=`mktemp`
currentDir=`pwd`
(cd /tmp; git clone https://git.gitorious.org/skalpel/skalpel.git $repoDir &>$compilationLog)
cd $repoDir/analysis-engines/standard-ml
autoconf &> $compilationLog
./configure &> $compilationLog
make mlton-bin &> $compilationLog

# this is created for the dead links output, this directory is removed at the end
mkdir -p $resultsDir

# run the analysis engine tests
echo "running analysis engine tests..."
analysisTestsLog=`mktemp`
$skalpelBin -d NO_COLOURS -b 2 $basisFile -c $analysisTestDir &> $analysisTestsLog

# check for any dead links in the webdemo
# NOTE: We can't currently do this here because we are not on the webserver,
#       therefore for the moment this is done via a web server cron job
# $testFrameworkDir/scripts/check-webdemo-links.sh > $resultsDir/$deadLinksTestFilename 2> $resultsDir/$deadLinksTestFilename-errors

# wait 5 minutes for other tests to be completed
# sleep 300

################################################################################
#      create the e-mail to send out to the Skalpel project developers
################################################################################

# create a new temporary file to put the e-mail into
mailFile=`mktemp`
echo "creating mail file at location $mailFile..."

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

echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                  Analysis Engine Compilation Diff (MLton)                  *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

diffOutput=`diff $masterDir/$compMltonTestMasterFilename $compilationLog`

if [ "${diffOutput}" = "" ]
then
echo "No difference between master file and output generated."  >> $mailFile
else
diff $masterDir/$compMltonTestMasterFilename $compilationLog >> $mailFile
fi


echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                 Analysis Engine Compilation Diff (Poly/ML)                 *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

diffOutput=`diff $masterDir/$compPolyTestMasterFilename $polyCompilationLog`

if [ "${diffOutput}" = "" ]
then
echo "No difference between master file and output generated."  >> $mailFile
else
diff $masterDir/$compPolyTestMasterFilename $polyCompilationLog >> $mailFile
fi

echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                          Analysis Engine Version                           *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e ""  >> $mailFile

echo `$skalpelBin -v` >> $mailFile

echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                         Website Broken Links Diff                          *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

# check that the webserver produced a log
if [ -f "$resultsDir/$deadLinksTestFilename" ]
then
    diff $masterDir/$deadLinksTestMasterFilename $resultsDir/$deadLinksTestFilename >> $mailFile
else
    echo "An error has been detected: The webserver has not produced a log of broken website links." >> $mailFile
fi


echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                         Analysis Engine Tests Diff                         *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

diffOutput=`diff $masterDir/$analysisTestMasterFilename $analysisTestsLog`

if [ "${diffOutput}" = "" ]
then
echo "No difference between master file and output generated."  >> $mailFile
else
diff $masterDir/$analysisTestMasterFilename $analysisTestsLog >> $mailFile
fi

echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                  Analysis Engine Compilation Log (MLton)                   *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

cat $compilationLog >> $mailFile

echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                 Analysis Engine Compilation Log (Poly/ML)                  *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

head -n 50 $polyCompilationLog >> $mailFile
echo "..." >> $mailFile
echo "..." >> $mailFile
echo "[ Intermediate part of log ommitted due to its size ]" >> $mailFile
echo "..." >> $mailFile
echo "..." >> $mailFile
tail -n 30 $polyCompilationLog >> $mailFile

echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                         Analysis Engine Tests Log                          *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

cat $analysisTestsLog >> $mailFile

echo -e "\n" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "*                          Website Broken Links Log                          *" >> $mailFile
echo -e "******************************************************************************" >> $mailFile
echo -e "" >> $mailFile

# check that the webserver produced a log
if [ -f "$resultsDir/$deadLinksTestFilename" ]
then
    cat $resultsDir/$deadLinksTestFilename >> $mailFile
else
    echo "An error has been detected: The webserver has not produced a log of broken website links." >> $mailFile
fi

cp $mailFile $resultsDir/test-results

# apparently you can't send mail to non-HW adresses with this. There's probably a way around that though.
cat $mailFile | mail -s "Skalpel daily test $date" jp95@macs.hw.ac.uk jbw@macs.hw.ac.uk

# remove the temporary files
rm -rf $polyRepoDir $repoDir $mailFile $polyCompilationLog $compilationLog $analysisTestsLog

