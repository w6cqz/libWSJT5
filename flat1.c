/* flat1.f -- translated by f2c (version 20100827).
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

/* Table of constant values */

static integer c__50 = 50;

/*      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*      contributions from additional authors.  WSJT is Open Source */
/*      software, licensed under the GNU General Public License V2 (GPL). */
/*      Source code and programming information may be found at */
/*      http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int flat1_(real *psavg, real *s2, integer *nh, integer *
	nsteps, integer *nhmax, integer *nsmax)
{
    /* System generated locals */
    integer s2_dim1, s2_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    static real x[8192];
    static integer ia, ib;
    static real tmp[33];
    static integer nsmo;
    extern /* Subroutine */ int pctile_(real *, real *, integer *, integer *, 
	    real *);

    /* Parameter adjustments */
    --psavg;
    s2_dim1 = *nhmax;
    s2_offset = 1 + s2_dim1;
    s2 -= s2_offset;

    /* Function Body */
    nsmo = 33;
    ia = nsmo / 2 + 1;
    ib = *nh - nsmo / 2 - 1;
    i__1 = ib;
    for (i__ = ia; i__ <= i__1; ++i__) {
	pctile_(&psavg[i__ - nsmo / 2], tmp, &nsmo, &c__50, &x[i__ - 1]);
    }
    i__1 = ia - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__ - 1] = x[ia - 1];
    }
    i__1 = *nh;
    for (i__ = ib + 1; i__ <= i__1; ++i__) {
	x[i__ - 1] = x[ib - 1];
    }
    i__1 = *nh;
    for (i__ = 1; i__ <= i__1; ++i__) {
	psavg[i__] /= x[i__ - 1];
	i__2 = *nsteps;
	for (j = 1; j <= i__2; ++j) {
	    s2[i__ + j * s2_dim1] /= x[i__ - 1];
	}
    }
    return 0;
} /* flat1_ */

