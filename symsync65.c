/* symsync65.f -- translated by f2c (version 20100827).
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
/* Subroutine */ int symsync65_(complex *c5, integer *n5, integer *k0, real *
	s, real *flip, real *pr, integer *kmax, integer *kpk, real *ccf, real 
	*smax)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1;
    complex q__1, q__2;

    /* Builtin functions */
    double r_imag(complex *);

    /* Local variables */
    static integer i__, j, k;
    static complex z__;
    static integer iz;
    static real sum;

    /* Parameter adjustments */
    --s;
    --c5;
    --pr;
    ccf -= -128;

    /* Function Body */
    z__.r = 0.f, z__.i = 0.f;
    for (i__ = 1; i__ <= 32; ++i__) {
	i__1 = i__;
	q__1.r = z__.r + c5[i__1].r, q__1.i = z__.i + c5[i__1].i;
	z__.r = q__1.r, z__.i = q__1.i;
    }
    s[1] = z__.r * z__.r + r_imag(&z__) * r_imag(&z__);
    *smax = s[1];
    i__1 = *n5;
    for (i__ = 33; i__ <= i__1; ++i__) {
	i__2 = i__;
	q__2.r = z__.r + c5[i__2].r, q__2.i = z__.i + c5[i__2].i;
	i__3 = i__ - 32;
	q__1.r = q__2.r - c5[i__3].r, q__1.i = q__2.i - c5[i__3].i;
	z__.r = q__1.r, z__.i = q__1.i;
	s[i__ - 31] = z__.r * z__.r + r_imag(&z__) * r_imag(&z__);
/* Computing MAX */
	r__1 = s[i__ - 31];
	*smax = dmax(r__1,*smax);
    }
    iz = *n5 - 31;
    *smax = 0.f;
    i__1 = *kmax;
    for (k = -(*kmax); k <= i__1; ++k) {
	sum = 0.f;
	for (i__ = 1; i__ <= 126; ++i__) {
	    j = (i__ - 1 << 5) + k + *k0;
	    if (j >= 1 && j <= iz) {
		sum += *flip * pr[i__] * s[j];
	    }
	}
	ccf[k] = sum;
	if (sum > *smax) {
	    *smax = sum;
	    *kpk = k;
	}
    }
    return 0;
} /* symsync65_ */

