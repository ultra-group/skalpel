#!/bin/bash
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

# Install script
set -ue

cd ${TRAVIS_BUILD_DIR}

echo "> Installing MLton (20130715)"

wget -q "https://sourceforge.net/projects/mlton/files/mlton/20130715/mlton-20130715-2.amd64-linux.tgz"
tar -xzf mlton-20130715-2.amd64-linux.tgz
rm mlton-20130715-2.amd64-linux.tgz
cd mlton-20130715-2
sudo make install
cd ${TRAVIS_BUILD_DIR}
rm -rf mlton-20130715-2

if [[ "${SML_IMPL}" = "polyml" ]] ; then
	echo "> Installing PolyML (5.7.1)"
	POLY_RELEASE_COMMIT=44b7b88e1a46757dfcddaab0166ca86c7024f198
	git clone https://github.com/polyml/polyml.git
	cd polyml
	git checkout "${POLY_RELEASE_COMMIT}" -b release-5-7-1
	./configure
	make
	make compiler
	sudo make install

	cd ${TRAVIS_BUILD_DIR}
	rm -rf polyml
fi
