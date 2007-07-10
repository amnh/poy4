#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([POY], [4.0 Beta], poy4@groups.google.com)

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_CC
AC_PROG_INSTALL

AC_SUBST(USEGRAPHICS)
AC_ARG_ENABLE([graphics], [AS_HELP_STRING([--enable-graphics],
              [Force POY to support graphical trees on screen. Valid options are none, ocaml, and tk. The default is none])], [USEGRAPHICS=$enableval], [USEGRAPHICS=none])

AC_SUBST(USEINTERFACE)
AC_ARG_ENABLE([interface], [AS_HELP_STRING([--enable-interface],
              [Force POY's user interface. Valid options are ncurses and readline. The default is ncurses.])],
              [USEINTERFACE=$enableval],
              [USEINTERFACE=ncurses])

AC_ARG_ENABLE([mpi], [AS_HELP_STRING([--enable-mpi=libname], 
              [Turns on parallel support using MPI, with the library name libname. When using MPI remember to set CC to mpicc.])], 
              [USEPARALLEL=true; MPILNAME=$enableval], 
              [USEPARALLEL=false])

AC_SUBST(USESHAREDHD)
AC_ARG_ENABLE([shared-hd], [AS_HELP_STRING([--enable-shared-hd],
              [Force POY to support live broadcast of file contents when running in parallel. The default is false.])], [USESHAREDHD=true], [USESHAREDHD=false])

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 test/Makefile])
AC_CONFIG_SUBDIRS([ocamlmpi
                   src])
AC_OUTPUT
