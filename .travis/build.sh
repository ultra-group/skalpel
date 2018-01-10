#!/bin/sh
#
# Copyright 2018 Christian Gregg
#
# Skalpel is a free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Skalpel is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
#

# Build Script
echo "Executing Build Script - Building for [${SML_IMPL}]"

BUILD_HOME="${TRAVIS_BUILD_DIR}/analysis-engines/standard-ml"
cd ${BUILD_HOME}

autoconf
./configure
make "${SML_IMPL}-bin"
sudo make install-core
