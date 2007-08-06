#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(FULL-PACKAGE-NAME, VERSION, BUG-REPORT-ADDRESS)
AC_CONFIG_SRCDIR([add.c])
AC_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CC

# What are we going to substitute in the Makefile
AC_SUBST(USEGRAPHICS)
AC_SUBST(USEINTERFACE)
AC_SUBST(USESHAREDHD)
AC_SUBST(USEPARALLEL)
AC_SUBST(USE_LONG_SEQUENCES)
AC_SUBST(USE_LARGE_ALPHABETS)
AC_SUBST(MPILNAME)
AC_SUBST(OCAML)
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLTOP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(LIBS)

# Checking the kind of graphics that we intend to support.
AC_ARG_ENABLE([graphics], [AS_HELP_STRING([--enable-graphics],
              [Force POY to support graphical trees on screen. Valid options are none, ocaml, and tk. The default is none])], [USEGRAPHICS=$enableval], [USEGRAPHICS=none])

AC_ARG_ENABLE([interface], [AS_HELP_STRING([--enable-interface],
              [Force POY's user interface. Valid options are ncurses, readline, and html. The default is ncurses.])],
              [USEINTERFACE=$enableval],
              [USEINTERFACE=ncurses])

AC_ARG_ENABLE([mpi], [AS_HELP_STRING([--enable-mpi=libname], 
              [Turns on parallel support using MPI, with the library name libname. When using MPI remember to set CC to mpicc.])], 
              [USEPARALLEL=true; MPILNAME=$enableval], 
              [USEPARALLEL=false])

AC_ARG_ENABLE([shared-hd], [AS_HELP_STRING([--enable-shared-hd],
              [Force POY to support live broadcast of file contents when running in parallel. The default is false.])], [USESHAREDHD=true], [USESHAREDHD=false])

AC_ARG_ENABLE([long-sequences], [AS_HELP_STRING([--enable-long-sequences], 
              [Enable the analysis of sequences longer than 16383 elements (The limit would be  2^31 or 2^63 for 32 bit and 64 bit architectures respectively). When using this option, more memory will be needed for the default search strategies. If you are analyzing fragments shorted than, say 12.000 bp, you are unlikely to need this option.])], 
              [USE_LONG_SEQUENCES=true], 
              [USE_LONG_SEQUENCES=false])

AC_ARG_ENABLE([large-alphabets], [AS_HELP_STRING([--enable-large-alphabets],
              [Enable the analysis of alphabets larger than 255 elements. When using this option, more memory will be needed to store each tree (4 or 8 times more for 32 and 64 bit computers respectively). This option is only needed for large alphabets if you use custom alphabets. Normal nucleotide, aminoacid, or morphological analyses are nor affected by this option.])],
              [USE_LARGE_ALPHABETS=true],
              [USE_LARGE_ALPHABETS=false])

# Checking for libraries according to the user's options.
AS_IF([test $USEINTERFACE = "ncurses"], [AC_CHECK_LIB([ncurses], [scrollok],
      [LIBS="-lncurses $LIBS"; AC_DEFINE(HAVE_LIBNCURSES)], [AC_MSG_FAILURE([The ncurses interface requires the ncurses library. Either install it, or choose the readline interface. See ./configure --help for more information.])])])

AS_IF([test $USEINTERFACE = "readline"], [AC_CHECK_LIB([readline], [readline],
      [LIBS="-lreadline $LIBS"; AC_DEFINE(HAVE_LIBREADLINE)], [AC_MSG_FAILURE([The ncurses interface requires the ncurses library. Either install it, or choose the readline interface. See ./configure --help for more information.])])])

AS_IF([test $USEPARALLEL = "true"], [AC_CHECK_LIB([$MPILNAME], [MPI_Init],
      [LIBS="-l$MPILNAME $LIBS"], [AC_MSG_FAILURE([In order to compile the parallel version, an implemention of MPI is required. However, I failed while checking for $MPILNAME.])])])

AC_CHECK_PROGS([OCAML], [ocaml])
AC_CHECK_PROGS([OCAMLC], [ocamlc.opt ocamlc])
AC_CHECK_PROGS([OCAMLYACC], [ocamlyacc])
AC_CHECK_PROGS([OCAMLLEX], [ocamllex])
AC_CHECK_PROGS([OCAMLOPT], [ocamlopt.opt ocamlopt])
AC_CHECK_PROGS([OCAMLDEP], [ocamldep])
AC_CHECK_PROGS([OCAMLTOP], [ocamlmktop])


case `../gnu/config.guess` in
    x86_64*)
        OCAMLOPT="${OCAMLOPT} -fno-PIC"
        ;;
    *)
        OCAMLOPT=${OCAMLOPT}
        ;;
esac

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h float.h limits.h malloc.h stddef.h stdint.h stdlib.h string.h strings.h sys/ioctl.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

# Checks for library functions.
AC_PROG_GCC_TRADITIONAL
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_CHECK_FUNCS([floor gettimeofday memset pow sqrt])

AC_CONFIG_FILES([Makefile
                 graphps/Makefile
                 grappa/Makefile])
AC_OUTPUT
