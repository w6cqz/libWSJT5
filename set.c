/* set.f -- translated by f2c (version 20100827).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/*      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*      contributions from additional authors.  WSJT is Open Source */
/*      software, licensed under the GNU General Public License V2 (GPL). */
/*      Source code and programming information may be found at */
/*      http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int set_(real *a, real *y, integer *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;

    /* Parameter adjustments */
    --y;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	y[i__] = *a;
    }
    return 0;
} /* set_ */

/* Subroutine */ int move_(real *x, real *y, integer *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;

    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	y[i__] = x[i__];
    }
    return 0;
} /* move_ */

/* Subroutine */ int zero_(real *x, integer *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__] = 0.f;
    }
    return 0;
} /* zero_ */

/* Subroutine */ int add_(real *a, real *b, real *c__, integer *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;

    /* Parameter adjustments */
    --c__;
    --b;
    --a;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__[i__] = a[i__] + b[i__];
    }
    return 0;
} /* add_ */

