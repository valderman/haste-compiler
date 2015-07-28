AC_INIT([Haskell time package], [1.4.0.2], [ashley@semantic.org], [time])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([lib/include/HsTime.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_ARG_WITH([gcc],[Gnu C compiler])
AC_ARG_WITH([compiler],[Haskell compiler])
AC_PROG_CC()

AC_CONFIG_HEADERS([lib/include/HsTimeConfig.h])

AC_CHECK_HEADERS([time.h])
AC_CHECK_FUNCS([gmtime_r localtime_r])

AC_STRUCT_TM
AC_STRUCT_TIMEZONE

FP_DECL_ALTZONE

AC_OUTPUT
