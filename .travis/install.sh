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

# Install script

echo "Installing MLton (20130715)"

set -v
wget -q "https://sourceforge.net/projects/mlton/files/mlton/20130715/mlton-20130715-2.amd64-linux.tgz"
mkdir -p mlton-install
tar -xzf mlton-20130715-2.amd64-linux.tgz
cd mlton-20130715-2
sudo make install
rm -rf mlton-20130715-2
