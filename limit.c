/* limit.f -- translated by f2c (version 20100827).
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

/* Common Block Declarations */

struct {
    integer nslim2;
} limcom_;

#define limcom_1 limcom_

/*      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*      contributions from additional authors.  WSJT is Open Source */
/*      software, licensed under the GNU General Public License V2 (GPL). */
/*      Source code and programming information may be found at */
/*      http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int limit_(real *x, integer *jz)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__;
    static real x1, sq, fac, rms, rms0, xlim;
    static logical noping;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    noping = FALSE_;
    xlim = 1e30f;
    if (limcom_1.nslim2 == 1) {
	xlim = 3.f;
    }
    if (limcom_1.nslim2 >= 2) {
	xlim = 1.f;
    }
    if (limcom_1.nslim2 >= 3) {
	noping = TRUE_;
    }
    sq = 0.f;
    i__1 = *jz;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sq += x[i__] * x[i__];
    }
    rms = sqrt(sq / *jz);
    rms0 = 14.5f;
    x1 = xlim * rms0;
    fac = 1.f / xlim;
    if (fac < 1.f) {
	fac = 1.f;
    }
    if (noping && rms > 20.f) {
	fac = .01f;
    }
/* Crude attempt at ping ex */
    i__1 = *jz;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (x[i__] < -x1) {
	    x[i__] = -x1;
	}
	if (x[i__] > x1) {
	    x[i__] = x1;
	}
	x[i__] = fac * x[i__];
    }
    return 0;
} /* limit_ */

