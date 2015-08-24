#ifndef __GHCAUTOCONF_H__
#define __GHCAUTOCONF_H__
/* mk/config.h.  Generated from config.h.in by configure.  */
/* mk/config.h.in.  Generated from configure.ac by autoheader.  */

/* The alignment of a `char'. */
#define ALIGNMENT_CHAR 1

/* The alignment of a `double'. */
#define ALIGNMENT_DOUBLE 8

/* The alignment of a `float'. */
#define ALIGNMENT_FLOAT 8

/* The alignment of a `int'. */
#define ALIGNMENT_INT 4

/* The alignment of a `long'. */
#define ALIGNMENT_LONG 4

/* The alignment of a `long long'. */
#define ALIGNMENT_LONG_LONG 8

/* The alignment of a `short'. */
#define ALIGNMENT_SHORT 2

/* The alignment of a `unsigned char'. */
#define ALIGNMENT_UNSIGNED_CHAR 1

/* The alignment of a `unsigned int'. */
#define ALIGNMENT_UNSIGNED_INT 4

/* The alignment of a `unsigned long'. */
#define ALIGNMENT_UNSIGNED_LONG 4

/* The alignment of a `unsigned long long'. */
#define ALIGNMENT_UNSIGNED_LONG_LONG 8

/* The alignment of a `unsigned short'. */
#define ALIGNMENT_UNSIGNED_SHORT 2

/* The alignment of a `void *'. */
#define ALIGNMENT_VOID_P 4

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to 1 if your processor stores words of floats with the most
   significant byte first */
/* #undef FLOAT_WORDS_BIGENDIAN */

/* Has visibility hidden */
#define HAS_VISIBILITY_HIDDEN 1

/* Define to 1 if you have /bin/sh. */
#define HAVE_BIN_SH 1

/* Define to 1 if you have the <ctype.h> header file. */
#define HAVE_CTYPE_H 1

/* Define to 1 if the system has the type `long long'. */
#define HAVE_LONG_LONG 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of `double', as computed by sizeof. */
#define SIZEOF_DOUBLE 8

/* The size of `float', as computed by sizeof. */
#define SIZEOF_FLOAT 4

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of `unsigned char', as computed by sizeof. */
#define SIZEOF_UNSIGNED_CHAR 1

/* The size of `unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT 4

/* The size of `unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG 4

/* The size of `unsigned long long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_LONG 8

/* The size of `unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT 2

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 4

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

#define TABLES_NEXT_TO_CODE 1
#endif /* __GHCAUTOCONF_H__ */
