#!/bin/bash

###############################################################
###############################################################
##
## Copyright 2010 Heriot-Watt University
## Copyright 2011 Heriot-Watt University
##
## This file is free software: you can redistribute it and/or modify
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
##
## Authors: Steven Shiells
## Date: December 2009
## Description: This is a shell script to run Skalpel, type 
##              error slicer for SML.
##
################################################################
################################################################

# We create the cmtotes binary
if ! make bin/cmtotes
then
    echo -e "\nError: Could not make cmtotes binary."
    exit
fi

# Current directory
cur_dir=`pwd`


# We create the file that holds the files needed by MLton to compile the slicer
mltondep=mlton-dependencies
> ${mltondep}


# We convert all cm files into mlb files
function cmtomlb {

    # The list of sources.cm occurring in the current directory or subdirectories
    cmfiles=`find . -name "sources.cm"`

    for cmfile in ${cmfiles};
    do

	rel_dir=`dirname ${cmfile}`
	dir=`cd ${rel_dir}; pwd`
	last_dir=`basename ${dir}`
	mlbfile=${dir}/sources.mlb
	tesfile=${dir}/sources.tes

	echo
	echo "converting ${cmfile} to ${mlbfile}..."

        # We add the copyright/license statements in the mlb file
	echo "(* Copyright 2010 Heriot-Watt University"                                >  ${mlbfile}
	echo " *"                                                                      >> ${mlbfile}
	echo " *"                                                                      >> ${mlbfile}
	echo " * This file is part of the ULTRA SML Type Error Slicer (SMLTES) -"      >> ${mlbfile}
	echo " * a Type Error Slicer for Standard ML written by the ULTRA Group of"    >> ${mlbfile}
	echo " * Heriot-Watt University, Edinburgh."                                   >> ${mlbfile}
	echo " *"                                                                      >> ${mlbfile}
	echo " * SMLTES is a free software: you can redistribute it and/or modify"     >> ${mlbfile}
	echo " * it under the terms of the GNU General Public License as published by" >> ${mlbfile}
	echo " * the Free Software Foundation, either version 3 of the License, or"    >> ${mlbfile}
	echo " * (at your option) any later version."                                  >> ${mlbfile}
	echo " *"                                                                      >> ${mlbfile}
	echo " * SMLTES is distributed in the hope that it will be useful,"            >> ${mlbfile}
	echo " * but WITHOUT ANY WARRANTY; without even the implied warranty of"       >> ${mlbfile}
	echo " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"        >> ${mlbfile}
	echo " * GNU General Public License for more details."                         >> ${mlbfile}
	echo " *"                                                                      >> ${mlbfile}
	echo " * You should have received a copy of the GNU General Public License"    >> ${mlbfile}
	echo " * along with SMLTES.  If not, see <http://www.gnu.org/licenses/>."      >> ${mlbfile}
	echo " *)"                                                                     >> ${mlbfile}
	echo                                                                           >> ${mlbfile}
	echo                                                                           >> ${mlbfile}

        # We add the SML/NJ libraries to the mlb file
	echo "local"                                             >> ${mlbfile}
	echo                                                     >> ${mlbfile}
	echo "  (* import Basis Library *)"                      >> ${mlbfile}
	echo "  \$(SML_LIB)/basis/basis.mlb"                     >> ${mlbfile}
	echo "  \$(SML_LIB)/basis/sml-nj.mlb"                    >> ${mlbfile}
	echo "  \$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb"           >> ${mlbfile}
	echo "  \$(SML_LIB)/smlnj-lib/Controls/controls-lib.mlb" >> ${mlbfile}
	echo "  \$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb"        >> ${mlbfile}
	echo "  \$(SML_LIB)/smlnj-lib/PP/pp-lib.mlb"             >> ${mlbfile}
	echo                                                     >> ${mlbfile}

        # We add our mlb files to the mlb file
	for file in $( grep "^  [^\$(].*\.cm$" ${cmfile} );
	do
	    echo ${file} | sed "s/^\(.*\)\.cm/  \1.mlb/"         >> ${mlbfile}
	done
	echo                                                     >> ${mlbfile}

        # We add the other files
	./bin/cmtotes ${cmfile} ${tesfile}
	cat ${tesfile} | sed 's/^\(.*\)$/  \1/g' | sed 's/\.grm/-mlton.grm/' | sed 's/\.lex/-mlton.lex/' | sed 's/PPP\.sml/PPP-mlton.sml/' >> ${tesfile}.tmp
	cat ${tesfile}.tmp | sed 's#^  \(.*\)$#'"${dir}/\1#g"              >> ${mltondep}
	cat ${tesfile}.tmp                                                 >> ${mlbfile}
	rm ${tesfile}.tmp

	echo                                                     >> ${mlbfile}
	echo "in"                                                >> ${mlbfile} 
	echo                                                     >> ${mlbfile}

        # We add the exported structures
	for str in $( grep "^  structure " ${cmfile} );
	do
	    if [ "structure" != "$str" ];
	    then
		echo "  structure ${str}"                        >> ${mlbfile}
	    fi
	done
	for str in $( grep "^  functor " ${cmfile} );
	do
	    if [ "functor" != "$str" ];
	    then
		echo "  functor ${str}"                          >> ${mlbfile}
	    fi
	done

	if [ ${cur_dir} == ${dir} ];
	then
	    echo                                                 >> ${mlbfile}
	    echo "  mlton-control.sml"                           >> ${mlbfile}
	fi

	echo                                                     >> ${mlbfile}
	echo "end"                                               >> ${mlbfile}

        #rm ${tesfile}

	echo "done."

    done
}


# Converts the top sources.cm and its dependencies into a file that can be used with Poly/ML
function cmtopoly {

    # MLton related directories
    mlton=`which mlton`
    mlton_bin=`dirname ${mlton}`
    mlton_dir=`cd ${mlton_bin}; cd ../..; pwd`
    # ${mlton_bin} is copied into FILES.sml

    # The list of sources.cm on which the top sources.cm followed by the top sources.cm itself
    cmfiles=`grep "^  [^(]*sources.cm" sources.cm`" sources.cm"

    # We create the file needed by Poly/ML to compile the slicer
    # This creation relies on the top cm file being well ordered
    polymlfile=FILES.sml
    >  ${polymlfile}
    echo "val mlton = \"${mlton_dir}\";"                                >> ${polymlfile}
    echo "val lib   = \"/usr/lib/mlton/sml\";"                          >> ${polymlfile}
    echo                                                                >> ${polymlfile}
    echo "(* SML/NJ LIBRARIES *)"                                       >> ${polymlfile}
    echo                                                                >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/ord-key-sig.sml\");"     >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/ord-map-sig.sml\");"     >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/ord-set-sig.sml\");"     >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/lib-base-sig.sml\");"    >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/lib-base.sml\");"        >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/binary-set-fn.sml\");"   >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/binary-map-fn.sml\");"   >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/splaytree-sig.sml\");"   >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/splaytree.sml\");"       >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/splay-map-fn.sml\");"    >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/splay-set-fn.sml\");"    >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/fifo-sig.sml\");"        >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/fifo.sml\");"            >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/int-list-set.sml\");"    >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/smlnj-lib/Util/redblack-set-fn.sml\");" >> ${polymlfile}

    echo                                                                >> ${polymlfile}
    echo "(* ML-YACC LIBRARIES *)"                                      >> ${polymlfile}
    echo                                                                >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/mlyacc-lib/base.sig\");"                >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/mlyacc-lib/join.sml\");"                >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/mlyacc-lib/lrtable.sml\");"             >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/mlyacc-lib/stream.sml\");"              >> ${polymlfile}
    echo "use (mlton ^ lib ^ \"/mlyacc-lib/parser2.sml\");"             >> ${polymlfile}
    echo                                                                >> ${polymlfile}
    echo "(* MY FILES *)"                                               >> ${polymlfile}

    # We add the files on which the slicer depends
    for cmfile in ${cmfiles};
    do
	rel_dir=`dirname ${cmfile}`
	dir=`cd ${rel_dir}; pwd`
	tesfile=${dir}/sources.tes

	echo
	echo "extracting dependencies from ${cmfile}..."

	./bin/cmtotes ${cmfile} ${tesfile}
	cat ${tesfile} | sed 's/^\(.*\)$/  \1/g' | sed 's/\.grm/-mlton.grm/' | sed 's/\.lex/-mlton.lex/' | sed 's/PPP\.sml/PPP-mlton.sml/' >> ${tesfile}.tmp
	echo                                                               >> ${polymlfile}
	cat ${tesfile}.tmp | sed 's#^  \(.*\)$#'"use \"${rel_dir}/\1\";#g" >> ${polymlfile}
	cat ${tesfile}.tmp | sed 's#^  \(.*\)$#'"${dir}/\1#g"              >> ${mltondep}
	rm ${tesfile}.tmp

	echo "done"
    done

    # We add the interface file
    echo "use \"mlton-control.sml.in\";"               >> ${polymlfile}

    # We add the code that export the interface function to slicer-poly.o
    echo                                               >> ${polymlfile}
    echo "TextIO.flushOut TextIO.stdOut;"              >> ${polymlfile}
    echo "PolyML.export(\"slicer-poly\", slicerPoly);" >> ${polymlfile}
}


if [ "$#" -gt 0 ]
then
    case $1 in
	cmtomlb)
	    cmtomlb
	    ;;
	cmtopoly)
	    cmtopoly
	;;
	*)
	    cmtomlb
	    ;;
    esac
else
    cmtomlb
fi


###################################################################
# Copyright 2010 Heriot-Watt University
#
#
# This file is part of the ULTRA SML Type Error Slicer (SMLTES) -
# a Type Error Slicer for Standard ML written by the ULTRA Group of
# Heriot-Watt University, Edinburgh.
#
# SMLTES is a free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SMLTES is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SMLTES.  If not, see <http://www.gnu.org/licenses/>.
#
#  o Authors:     Vincent Rahli
#  o Affiliation: Heriot-Watt University, MACS
#  o Date:        July 2010
#  o File name:   cmtomlb.sh
#  o Description: this script recursively transforms the .cm files
#      present in the current directory (and the sub-directories)
#      into .mlb files.
###################################################################