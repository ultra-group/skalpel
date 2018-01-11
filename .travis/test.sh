#!/bin/bash
#
# Copyright 2018 Christian Gregg
#
# Skalpel is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
#

set -eu

BUILD_DIR="${TRAVIS_BUILD_DIR}"
SKALPEL="${BUILD_DIR}/analysis-engines/standard-ml/bin/skalpel"
BASIS_FILE="${BUILD_DIR}/lib/basis.sml"
TESTS_DIR="${BUILD_DIR}/testing/analysis-engine-tests/standard-ml"

# run the analysis engine tests
set -x
${SKALPEL} -d NO_COLOURS -b 2 ${BASIS_FILE} -c ${TESTS_DIR}

