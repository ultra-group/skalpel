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
## Description: A wrapper script for check-website-links.sh. This
##              wrapper script is executed by a cron job.
##
###############################################################
###############################################################

#!/bin/bash

# the current date in the format YEAR-MONTH-DAY-HOUR-MINUTE
date=`date '+%Y-%m-%d-%H-%M'`

repoDir='/u1/pg/jp95/repos/skalpel'

# directory for the skalpel binary
skalpelBin="$repoDir/analysis-engines/standard-ml/bin/skalpel"

# directory where the analysis engine test files are stored
analysisTestDir="$repoDir/testing/analysis-engine-tests/standard-ml/"

# director for the test framework
testFrameworkDir="$repoDir/testing"

# names of the files which are output
deadLinksTestFilename="skalpel-dead-links-$date"

# createt the output directory
outputDir="$repoDir/testing/test-results/$date"
mkdir -p $outputDir

# check for any dead links in the website
# NOTE: We can't currently do this here because we are not on the webserver,
#       therefore for the moment this is done via a web server cron job
$testFrameworkDir/scripts/check-website-links.sh > $outputDir/$deadLinksTestFilename 2> $outputDir/$deadLinksTestFilename-errors