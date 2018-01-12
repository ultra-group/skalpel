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

if [[ "${SML_IMPL}" = "mlton" ]] ; then
	echo "> Installing MLton (20130715)"

	cd ${TRAVIS_BUILD_DIR}
	wget -q "https://sourceforge.net/projects/mlton/files/mlton/20130715/mlton-20130715-2.amd64-linux.tgz"
	tar -xzf mlton-20130715-2.amd64-linux.tgz
	rm mlton-20130715-2.amd64-linux.tgz
	cd mlton-20130715-2
	sudo make install
	cd ${TRAVIS_BUILD_DIR}
	rm -rf mlton-20130715-2
elif [[ "${SML_IMPL}" = "smlnj" ]] ; then
	echo "> Installing SML/NJ (110.82)"
	# SML/NJ is 32-bit, need some stuff to enable travis to run 32-bit binaries
	sudo apt-get install -y gcc-multilib g++-multilib lib32ncurses5 lib32z1 lib32bz2-1.0

	sudo mkdir /usr/local/share/smlnj
	cd /usr/local/share/smlnj
	sudo wget -q "http://smlnj.cs.uchicago.edu/dist/working/110.82/config.tgz"
	sudo gunzip <config.tgz | sudo tar xf -
	sudo /bin/bash -c 'echo "request heap2asm" >> config/targets'
	sudo config/install.sh
	sudo cp -fpR bin/. /usr/local/bin/
else
	echo "SML_IMPL [${SML_IMPL}] not reconised."
	exit 1
fi
