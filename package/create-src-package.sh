###################################################################
# Copyright 2010 2011 John Pirie
# Copyright 2011 2013 Heriot-Watt University
#
# SKALPEL is a free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SKALPEL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SKALPEL.  If not, see <http://www.gnu.org/licenses/>.
#
#  o Authors: John Pirie o Affiliation: Heriot-Watt University, MACS o
#  Date: January 2011 o File name: create-src-package.sh o
#  Description: This script will generate source RPMs, binary RPMs, a
#               source archive, a debian binary archive, a debian source archive,
#               and a mac package which can be used for a new release of Skalpel.
###################################################################

#!/bin/bash

# the version of the source code is stored here
VERSION=0.8

if [ -d skalpel-$VERSION ]
then
    echo "File skalpel-$VERSION already exists in this directory. If this is an old version, delete it."
    exit 1
fi

mkdir -p skalpel-$VERSION

echo "version number set to ${VERSION}"

################################################################################
#                             copy necessary files                             #
################################################################################

# documentation
echo "copying documentation..."
mkdir -p						 skalpel-$VERSION/documentation/
cp ../documentation/end-user/user-guide/user-guide.pdf	 skalpel-$VERSION/documentation/
cp ../documentation/end-user/info-pages/Skalpel.info	 skalpel-$VERSION/documentation/
cp ../documentation/end-user/man-pages/skalpel.1	 skalpel-$VERSION/documentation/
cp ../README						 skalpel-$VERSION/
cp ../COPYING						 skalpel-$VERSION/


# user interface
echo "copying user interfaces..."
mkdir -p						 skalpel-$VERSION/front-ends/emacs
cp ../front-ends/emacs/SKALPEL-HELP			 skalpel-$VERSION/front-ends/emacs/
cp ../front-ends/emacs/skalpel-config.el		 skalpel-$VERSION/front-ends/emacs/
cp ../front-ends/emacs/skalpel-main.el			 skalpel-$VERSION/front-ends/emacs/
cp ../front-ends/emacs/skalpel-menu.el			 skalpel-$VERSION/front-ends/emacs/

# test database
echo "copying the test database..."
mkdir -p skalpel-$VERSION/testing/analysis-engine-tests/standard-ml/
cp -r ../testing/analysis-engine-tests/standard-ml/*    skalpel-$VERSION/testing/analysis-engine-tests/standard-ml/

# analysis-engine
echo "copying analysis-engine..."
mkdir -p skalpel-$VERSION/
find ../analysis-engines/standard-ml -name "*.sml" | grep -v ".cm/"  | cpio -pd skalpel-$VERSION/analysis-engines
find ../analysis-engines/standard-ml -name "*.sig" | grep -v ".cm/"  | cpio -pd skalpel-$VERSION/analysis-engines
find ../analysis-engines/standard-ml -name "*.mlb" | grep -v ".cm/"  | cpio -pd skalpel-$VERSION/analysis-engines
find ../analysis-engines/standard-ml -name "*.lex" | grep -v ".cm/"  | cpio -pd skalpel-$VERSION/analysis-engines
find ../analysis-engines/standard-ml -name "*.grm" | grep -v ".cm/"  | cpio -pd skalpel-$VERSION/analysis-engines
find ../analysis-engines/standard-ml -name "*.cm"  | grep -v ".cm/"  | cpio -pd skalpel-$VERSION/analysis-engines
cp ../analysis-engines/standard-ml/ChangeLog             skalpel-$VERSION/analysis-engines/standard-ml/
cp ../analysis-engines/standard-ml/configure.ac          skalpel-$VERSION/analysis-engines/standard-ml/
cp ../analysis-engines/standard-ml/Makefile.in           skalpel-$VERSION/analysis-engines/standard-ml/
cp ../analysis-engines/standard-ml/cmtomlb.sh            skalpel-$VERSION/analysis-engines/standard-ml/
cp ../analysis-engines/standard-ml/mlton-control.sml.in  skalpel-$VERSION/analysis-engines/standard-ml/

# basis
echo "copying the basis..."
mkdir -p skalpel-$VERSION/lib
cp ../lib/basis.sml skalpel-$VERSION/lib/

################################################################################
#                       create source archive (.tar.gz)                        #
################################################################################

echo "creating archive..."
tar -czf skalpel_${VERSION}-src.tar.gz skalpel-$VERSION

################################################################################
#                  create debian source archive (-src.tar.gz)                  #
################################################################################

DSCFILE="skalpel_${VERSION}.dsc"

> ${DSCFILE}
echo "Format: 1.0"                               >> ${DSCFILE}
echo "Source: skalpel"                           >> ${DSCFILE}
echo "Binary: skalpel"                           >> ${DSCFILE}
echo "Architecture: all"                         >> ${DSCFILE}
echo "Version: ${VERSION}"                       >> ${DSCFILE}
echo "Maintainer: John Pirie jpirie23@gmail.com" >> ${DSCFILE}
echo                                             >> ${DSCFILE}
echo "Standards-Version: 3.8.0"                  >> ${DSCFILE}
echo                                             >> ${DSCFILE}
echo "Files:"                                    >> ${DSCFILE}
echo " skalpel_${VERSION}.orig.tar.gz"           >> ${DSCFILE}

echo "creating debian source package..."
mkdir -p skalpel-${VERSION}/debian
cp debian/skalpel.in/debian/changelog     skalpel-${VERSION}/debian
cp debian/skalpel.in/debian/control       skalpel-${VERSION}/debian
cp debian/skalpel.in/debian/rules         skalpel-${VERSION}/debian
mkdir -p debian-src
tar -czf debian-src/skalpel_${VERSION}.orig.tar.gz skalpel-${VERSION}
cp ${DSCFILE} debian-src

# we need to move the files so that the extraction of the debian source
# package gets the correct name
mv skalpel-${VERSION} skalpel-${VERSION}.moved
mv debian-src skalpel-${VERSION}
tar -czf skalpel_${VERSION}-debian-src.tar.gz skalpel-${VERSION}
rm -rf skalpel-${VERSION} # the version we don't need
mv skalpel-${VERSION}.moved skalpel-${VERSION}

echo "checking which packages to build..."

################################################################################
#                   create rpm and source rpm (.rpm, .srpm)                    #
################################################################################

rpmBuildCheck=`rpmbuild --version 2> /dev/null`

if [ "$rpmBuildCheck" != "" ]
then
    echo "rpmbuild has been found!"

    # in fedora we can use devtree which does all this for us
    # this is not installed on these machines, so this is a way around that
    echo "creating red hat packages (RPM and SRPM)..."

    # make the directories that rpmbuild requires
    mkdir tmpbuild/
    mkdir tmpbuild/SOURCES tmpbuild/SRPMS tmpbuild/BUILD

    # copy the source archive to the SOURCES folder so that we can make
    # the source rpm from that
    cp skalpel_$VERSION-src.tar.gz tmpbuild/SOURCES

    # the build process of rpmbuild runs a shell at the location of BUILD,
    # this is a quick hack to get around that problem.
    mkdir -p tmpbuild/RPMS/i386/
    mkdir -p tmpbuild/BUILD/tmpbuild/SOURCES/
    cp skalpel_$VERSION-src.tar.gz tmpbuild/BUILD/tmpbuild/SOURCES/

    # build the binary and the source packages. Unfortunately, it seems to
    # want to generate the mlton binary twice.
    currentDir=`echo \`pwd\``
    rpmbuild -ba --buildroot $currentDir/build-rpm/ --define '_topdir tmpbuild' red-hat/skalpel.spec

    # copy the files to the right directory
    for FILE in `find tmpbuild/ -type f | grep "\.rpm"`; do mv $FILE ./; done
else
    echo "rpmcheck has NOT been detected - will not generate .rpm and .srpm"
fi

################################################################################
#                         create debian package (.deb)                         #
################################################################################

dpkgBuildCheck=`dpkg --version 2> /dev/null`

if [ "$dpkgBuildCheck" != "" ]
then
    echo "dpkg has been found!"
    echo "TODO - makefile should be called to create deb file"
else
    echo "dpkg has NOT been detected - .deb will not be created"
fi

################################################################################
#                          create mac package (.bom)                           #
################################################################################

bomCheck=`mkbom 2> /dev/null`
bomOk=false;
if [ "$bomCheck" != "mkbom: command not found" ]
then
    echo "mkbom found.";
    bomOk=true;
else
    echo "mkbom not found. Mac pkg cannot be created";
fi

paxOk=false;
paxCheck=`pax 2> /dev/null`
if [ "$paxCheck" != "pax: command not found" ]
then
    echo "pax found.";
    paxOk=true;
else
    echo "pax not found. Mac pkg cannot be created";
fi

if [[ "$paxOk" == true && "$bomOk" == true ]]
then
    echo "building Mac pkg script";
    # <insert build instructions here>
fi

################################################################################
#                                   clean up                                   #
################################################################################

echo "cleaning up..."
rm -rf tmpbuild/
rm -r skalpel-$VERSION

echo "complete."

