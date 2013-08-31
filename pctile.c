/* pctile.f -- translated by f2c (version 20100827).
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

/*     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*     contributions from additional authors.  WSJT is Open Source */
/*     software, licensed under the GNU General Public License V2 (GPL). */
/*     Source code and programming information may be found at */
/*     http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int pctile_(real *x, real *tmp, integer *nmax, integer *npct,
	 real *xpct)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    integer i_nint(real *);

    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int sort_(integer *, real *);

    /* Parameter adjustments */
    --tmp;
    --x;

    /* Function Body */
    i__1 = *nmax;
    for (i__ = 1; i__ <= i__1; ++i__) {
	tmp[i__] = x[i__];
    }
    sort_(nmax, &tmp[1]);
    r__1 = *nmax * .01f * *npct;
    j = i_nint(&r__1);
    if (j < 1) {
	j = 1;
    }
    *xpct = tmp[j];
    return 0;
} /* pctile_ */

