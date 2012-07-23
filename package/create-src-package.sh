###################################################################
# Copyright 2010 2011 John Pirie
# Copyright 2011 Heriot-Watt University
#
#
# This file is part of the ULTRA SML Type Error Slicer (SKALPEL) -
# a Type Error Slicer for Standard ML written by the ULTRA Group of
# Heriot-Watt University, Edinburgh.
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
#  o Authors:     John Pirie
#  o Affiliation: Heriot-Watt University, MACS
#  o Date:        January 2011
#  o File name:   create-src-package.sh
#  o Description: This script will generate source RPMs, binary RPMs,
#                 and a source archive which can be used for a new
#                 release of the slicer.
###################################################################

#!/bin/bash

# the version of the source code is stored here
VERSION=0.7

mkdir -p skalpel-$VERSION

echo "version number set to ${VERSION}"

echo "calling 'make cmtomlb' in ../implementation/tes-seq/ ..."
(cd ../implementation/tes-seq/; make cmtomlb)

echo "copying configuration and makefile.in..."
(cd ../implementation/; autoconf)
cp ../implementation/configure skalpel-$VERSION/
cp ../implementation/Makefile.in skalpel-$VERSION/

########## documentation ##########

echo "copying documentation..."
mkdir -p skalpel-$VERSION/user-guide/

# user guide
mkdir -p skalpel-$VERSION/user-guide/
cp ../documentation/user-guide/user-guide.tex skalpel-$VERSION/user-guide/

# readme
mkdir -p skalpel-$VERSION/readme
cp ../documentation/readme/README skalpel-$VERSION/readme/
makeinfo ../documentation/user-guide/info/emacsmanual.texi
cp Skalpel.info skalpel-$VERSION/readme/

########## user interfaces ##########

echo "copying user interfaces..."

mkdir -p skalpel-$VERSION/command-line
cp ../ui/command-line/skalpel                skalpel-$VERSION/command-line/
cp ../ui/command-line/skalpel.1              skalpel-$VERSION/command-line/
cp ../ui/command-line/skalpel-bin.1          skalpel-$VERSION/command-line/
cp ../ui/command-line/skalpel-legend         skalpel-$VERSION/command-line/
cp ../ui/command-line/skalpel-legend.1       skalpel-$VERSION/command-line/
cp ../ui/command-line/skalpel-perl-to-bash   skalpel-$VERSION/command-line/
cp ../ui/command-line/skalpel-perl-to-bash.1 skalpel-$VERSION/command-line/

mkdir -p skalpel-$VERSION/EmacsUI
cp ../ui/EmacsUI/SKALPEL-HELP                   skalpel-$VERSION/EmacsUI/
cp ../ui/EmacsUI/skalpel-config.el              skalpel-$VERSION/EmacsUI/
cp ../ui/EmacsUI/skalpel-debug-utils.el         skalpel-$VERSION/EmacsUI/
cp ../ui/EmacsUI/skalpel-main.el                skalpel-$VERSION/EmacsUI/

# fix the problem where the emacs.el file is looking at the wrong filename for the slicer binary
cat skalpel-$VERSION/EmacsUI/skalpel-main.el | sed 's/\(defvar skalpel-slicer-bin \)\"slicer\"/\1\"skalpel-bin\"/' > skalpel-main2.el; mv skalpel-main2.el skalpel-$VERSION/EmacsUI/skalpel-main.el

cp ../ui/EmacsUI/skalpel-menu.el               skalpel-$VERSION/EmacsUI/
cp shared/skalpel-emacs/skalpel-emacs.1 skalpel-$VERSION/EmacsUI/

########## database ##########

echo "copying the test database..."

mkdir -p skalpel-$VERSION/database/
cp ../implementation/database/code*.sml    skalpel-$VERSION/database/
cp ../implementation/database/output*.html skalpel-$VERSION/database/
cp ../implementation/database/test*.sml    skalpel-$VERSION/database/

########## implementation ##########

echo "copying tes-seq..."

mkdir -p skalpel-$VERSION/tes-seq

# files in the directory itself
cp ../implementation/tes-seq/ChangeLog            skalpel-$VERSION/tes-seq/
### Why do we provide this cmtomlb.sh?
### It would make sense to provide it if we were to provide the .cm files
### as well.
#cp ../implementation/tes-seq/cmtomlb.sh           skalpel-$VERSION/tes-seq/
### Why do we provide this mlton-control.sml, it should be generated
### from the initial one?
#cp ../implementation/tes-seq/mlton-control.sml    skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/mlton-control.sml.in skalpel-$VERSION/tes-seq/
### Why do we provide PGOps?
#cp ../implementation/tes-seq/PGOps.sml            skalpel-$VERSION/tes-seq/
### Why do we provide this README?
cp ../implementation/tes-seq/README               skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/RunSlicer.sig        skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/RunSlicer.sml        skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/SlicerOptArgs.sig    skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/SlicerOptArgs.sml    skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/sources.cm           skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/sources.mlb          skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/Tester.sig           skalpel-$VERSION/tes-seq/
cp ../implementation/tes-seq/Tester.sml           skalpel-$VERSION/tes-seq/

#ast
mkdir -p skalpel-$VERSION/tes-seq/ast
cp ../implementation/tes-seq/ast/AstMLB.sml  skalpel-$VERSION/tes-seq/ast/
cp ../implementation/tes-seq/ast/AstSML.sig  skalpel-$VERSION/tes-seq/ast/
cp ../implementation/tes-seq/ast/AstSML.sml  skalpel-$VERSION/tes-seq/ast/
cp ../implementation/tes-seq/ast/AstTest.sig skalpel-$VERSION/tes-seq/ast/
cp ../implementation/tes-seq/ast/AstTest.sml skalpel-$VERSION/tes-seq/ast/
cp ../implementation/tes-seq/ast/sources.mlb skalpel-$VERSION/tes-seq/ast/

# constraint
mkdir -p skalpel-$VERSION/tes-seq/constraint
cp ../implementation/tes-seq/constraint/ClassId.sig skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/ClassId.sml skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/ConsId.sig  skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/ConsId.sml  skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Env.sig     skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Env.sml     skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Expans.sig  skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Expans.sml  skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Poly.sig    skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Poly.sml    skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Ty.sig      skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/Ty.sml      skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/ExtLab.sig  skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/ExtLab.sml  skalpel-$VERSION/tes-seq/constraint/
cp ../implementation/tes-seq/constraint/sources.mlb skalpel-$VERSION/tes-seq/constraint/

# enum
mkdir -p skalpel-$VERSION/tes-seq/enum
cp ../implementation/tes-seq/enum/Enumeration.sig skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Enumeration.sml skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Filter.sig skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Filter.sml skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Fresh.sig skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Fresh.sml skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Minimisation.sig skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Minimisation.sml skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/SearchSpace.sig skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/SearchSpace.sml skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/State.sig skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/StateEnv.sml skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Unification.sig skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/Unification.sml skalpel-$VERSION/tes-seq/enum/
cp ../implementation/tes-seq/enum/sources.mlb skalpel-$VERSION/tes-seq/enum/

#error
mkdir -p skalpel-$VERSION/tes-seq/error
cp ../implementation/tes-seq/error/Error.sig skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/Error.sml skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/ErrorKind.sig skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/ErrorKind.sml skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/ExtReg.sig skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/ExtReg.sml skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/Html.sig skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/Html.sml skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/Tag.sig skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/Tag.sml skalpel-$VERSION/tes-seq/error/
cp ../implementation/tes-seq/error/sources.mlb skalpel-$VERSION/tes-seq/error/

# generation
mkdir -p skalpel-$VERSION/tes-seq/generation
cp ../implementation/tes-seq/generation/ValuePol.sig skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/ValuePol.sml skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/Analyze.sig  skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/Analyze.sml  skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/Name.sig     skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/Name.sml     skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/Op.sig       skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/Op.sml       skalpel-$VERSION/tes-seq/generation/
cp ../implementation/tes-seq/generation/sources.mlb  skalpel-$VERSION/tes-seq/generation/

# parse
mkdir -p skalpel-$VERSION/tes-seq/parse
cp ../implementation/tes-seq/parse/Comment.sig   skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/Comment.sml   skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/Infix.sig     skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/Infix.sml     skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/ParseDefs.sig skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/ParseDefs.sml skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/LexDefs.sig   skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/LexDefs.sml   skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/ML.grm        skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/ML.lex        skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/MLB.grm       skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/MLB.lex       skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/Parser.sig    skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/Parser.sml    skalpel-$VERSION/tes-seq/parse/
cp ../implementation/tes-seq/parse/sources.mlb   skalpel-$VERSION/tes-seq/parse/

# ppp
mkdir -p skalpel-$VERSION/tes-seq/ppp
cp ../implementation/tes-seq/ppp/PPP-mlton.sml skalpel-$VERSION/tes-seq/ppp/
cp ../implementation/tes-seq/ppp/PPP.sig skalpel-$VERSION/tes-seq/ppp/
cp ../implementation/tes-seq/ppp/PPP.sml skalpel-$VERSION/tes-seq/ppp/
cp ../implementation/tes-seq/ppp/sources.mlb skalpel-$VERSION/tes-seq/ppp

# region
mkdir -p skalpel-$VERSION/tes-seq/region/
cp ../implementation/tes-seq/region/Region.sig skalpel-$VERSION/tes-seq/region/
cp ../implementation/tes-seq/region/Region.sml skalpel-$VERSION/tes-seq/region/
cp ../implementation/tes-seq/region/sources.mlb skalpel-$VERSION/tes-seq/region/

# sets
mkdir -p skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/Ident.sig skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/Ident.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/Label.sig skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/Label.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/LongId.sig skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/LongId.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdIdl.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdId.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdKey.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdLabLid.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdLid.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdSet.sig skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdSet.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/OrdStr.sml skalpel-$VERSION/tes-seq/sets/
cp ../implementation/tes-seq/sets/sources.mlb skalpel-$VERSION/tes-seq/sets/

# slicing
mkdir -p skalpel-$VERSION/tes-seq/slicing
cp ../implementation/tes-seq/slicing/Slice.sig skalpel-$VERSION/tes-seq/slicing/
cp ../implementation/tes-seq/slicing/Slice.sml skalpel-$VERSION/tes-seq/slicing/
cp ../implementation/tes-seq/slicing/sources.mlb skalpel-$VERSION/tes-seq/slicing/

# utils
mkdir -p skalpel-$VERSION/tes-seq/utils
cp ../implementation/tes-seq/utils/Debug.sig skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/Debug.sml skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/ErrorHandler.sig skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/ErrorHandler.sml skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/Solution.sig skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/Solution.sml skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/SymbSlice.sml skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/Tools.sig skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/Tools.sml skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/VTimer.sig skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/VTimer.sml skalpel-$VERSION/tes-seq/utils/
cp ../implementation/tes-seq/utils/sources.mlb skalpel-$VERSION/tes-seq/utils/

########## package ##########
echo "copying extra documentation..."
mkdir -p skalpel-$VERSION/package/shared/skalpel/
cp shared/skalpel/README skalpel-$VERSION/package/shared/skalpel/
cp shared/COMPILE skalpel-$VERSION/
cp shared/COPYING skalpel-$VERSION/

########## basis.sml ##########

echo "copying the basis..."
mkdir -p skalpel-$VERSION/lib
cp ../implementation/lib/basis.sml skalpel-$VERSION/lib/

echo "creating archive..."
tar -czf skalpel_${VERSION}-src.tar.gz skalpel-$VERSION

########## create debian package ##########

DSCFILE="skalpel_${VERSION}.dsc"

> ${DSCFILE}
echo "Format: 1.0"                               >> ${DSCFILE}
echo "Source: skalpel"                            >> ${DSCFILE}
echo "Binary: skalpel"                            >> ${DSCFILE}
echo "Architecture: all"                         >> ${DSCFILE}
echo "Version: ${VERSION}"                       >> ${DSCFILE}
echo "Maintainer: John Pirie jpirie23@gmail.com" >> ${DSCFILE}
echo                                             >> ${DSCFILE}
echo "Standards-Version: 3.8.0"                  >> ${DSCFILE}
echo                                             >> ${DSCFILE}
echo "Files:"                                    >> ${DSCFILE}
echo " skalpel_${VERSION}.orig.tar.gz"            >> ${DSCFILE}

echo "creating debian source package..."
mkdir -p skalpel-${VERSION}/debian
cp debian-bin-package/skalpel.in/debian/changelog      skalpel-${VERSION}/debian
cp debian-bin-package/skalpel.in/debian/control        skalpel-${VERSION}/debian
cp other-packaging-files/rules                        skalpel-${VERSION}/debian
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

######### check for rpmbuild ############
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
    rpmbuild -ba --buildroot $currentDir/build-rpm/ --define '_topdir tmpbuild' other-packaging-files/skalpel.spec

    # copy the files to the right directory
    for FILE in `find tmpbuild/ -type f | grep "\.rpm"`; do mv $FILE ./; done
else
    echo "rpmcheck has NOT been detected - will not generate .rpm and .srpm"
fi

######### check for dpkg ############
dpkgBuildCheck=`dpkg --version 2> /dev/null`

if [ "$dpkgBuildCheck" != "" ]
then
    echo "dpkg has been found!"
    echo "TODO - makefile should be called to create deb file"
else
    echo "dpkg has NOT been detected - .deb will not be created"
fi

### please do not touch this section for the time being! - ta, Scott ########
######### check for Mac pkg dependencies ############
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

## Sorry, I've changed the following if_then_else because it was breaking the script, vincent.
if [[ "$paxOk" == true && "$bomOk" == true ]]
then
    echo "building Mac pkg script";
	# build Mac pkg script
fi

echo "cleaning up..."
rm -rf tmpbuild/
rm -r skalpel-$VERSION

echo "complete."

