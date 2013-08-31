/* ps.f -- translated by f2c (version 20100827).
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
/* Subroutine */ int ps_(real *dat, integer *nfft, real *s, integer *ical, 
	char *wisfile, ftnlen wisfile_len)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;
    static complex equiv_0[8193];

    /* Builtin functions */
    double r_imag(complex *);

    /* Local variables */
#define c__ (equiv_0)
    static integer i__;
#define x ((real *)equiv_0)
    static integer nh;
    static real fac;
    extern /* Subroutine */ int xfft_(real *, integer *, integer *, char *, 
	    ftnlen);

    /* Parameter adjustments */
    --dat;
    --s;

    /* Function Body */
    nh = *nfft / 2;
    i__1 = *nfft;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__ - 1] = dat[i__] / 128.f;
/* ### Why 128 ?? */
    }
    xfft_(x, nfft, ical, wisfile, (ftnlen)255);
    fac = 1.f / *nfft;
    i__1 = nh;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
/* Computing 2nd power */
	r__1 = c__[i__2].r;
/* Computing 2nd power */
	r__2 = r_imag(&c__[i__]);
	s[i__] = fac * (r__1 * r__1 + r__2 * r__2);
    }
    return 0;
} /* ps_ */

#undef x
#undef c__


