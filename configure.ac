dnl
dnl Configuring DataShovel
dnl Process this file with autoconf to generate 'configure'.
dnl $Id: configure.ac,v 0.1,2005/06/13 10:44:12$

AC_INIT(gauche-rdf, 0.1, keiji0@gmail.com)

AC_PATH_PROGS([GOSH], gosh)
AC_PATH_PROGS([GAUCHE_CONFIG], gauche-config)

dnl Check for other programs.
AC_PROG_INSTALL

if test X$GOSH = X; then 
  AC_MSG_ERROR([Cannot find Gauche installed.  Please install Gauche-0.8.6 or later, and make sure to set PATH properly.])
fi

dnl Add more test

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile src/Makefile)
