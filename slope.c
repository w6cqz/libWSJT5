/* slope.f -- translated by f2c (version 20100827).
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
/* Subroutine */ int slope_(real *y, integer *npts, real *xpk)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static real a, b;
    static integer i__;
    static real x[100], sumw, sumx, sumy, sumx2, sumy2, delta, sumxy;

/*     Remove best-fit slope from data in y(i).  When fitting the */
/*     straight line, ignore the peak around xpk +/- 2. */
    /* Parameter adjustments */
    --y;

    /* Function Body */
    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__ - 1] = (real) i__;
    }
    sumw = 0.f;
    sumx = 0.f;
    sumy = 0.f;
    sumx2 = 0.f;
    sumxy = 0.f;
    sumy2 = 0.f;
    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((r__1 = i__ - *xpk, dabs(r__1)) > 2.f) {
	    sumw += 1.f;
	    sumx += x[i__ - 1];
	    sumy += y[i__];
/* Computing 2nd power */
	    r__1 = x[i__ - 1];
	    sumx2 += r__1 * r__1;
	    sumxy += x[i__ - 1] * y[i__];
/* Computing 2nd power */
	    r__1 = y[i__];
	    sumy2 += r__1 * r__1;
	}
    }
/* Computing 2nd power */
    r__1 = sumx;
    delta = sumw * sumx2 - r__1 * r__1;
    a = (sumx2 * sumy - sumx * sumxy) / delta;
    b = (sumw * sumxy - sumx * sumy) / delta;
    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	y[i__] -= a + b * x[i__ - 1];
    }
    return 0;
} /* slope_ */

