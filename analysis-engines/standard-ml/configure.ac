#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(SMLTES, 0.6, rahli@macs.hw.ac.uk)

# Checks for programs.
#AC_PROG_LEX
AC_PROG_MAKE_SET

AC_CHECK_PROG(HAVE_MLLEX, mllex, yes, no)
if test x"$HAVE_MLLEX" = "xno" ; then
   AC_MSG_ERROR([ mllex should be installed to compile SMLTES ])
fi
AC_CHECK_PROG(HAVE_MLYACC, mlyacc, yes, no)
if test x"$HAVE_MLYACC" = "xno" ; then
   AC_MSG_ERROR([ mlyacc should be installed to compile SMLTES ])
fi
AC_CHECK_PROG(HAVE_MLTON, mlton, yes, no)
if test x"$HAVE_MLTON" = "xno" ; then
   AC_MSG_ERROR([ mlton should be installed to compile SMLTES ])
fi

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_OUTPUT