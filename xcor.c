/* xcor.f -- translated by f2c (version 20100827).
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
    real pr[135];
    integer mdat[126], mref[252]	/* was [126][2] */, mdat2[126], mref2[
	    252]	/* was [126][2] */;
} prcom_;

#define prcom_1 prcom_

struct {
    integer nclip;
} clipcom_;

#define clipcom_1 clipcom_

/* Table of constant values */

static integer c__50 = 50;

/*      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*      contributions from additional authors.  WSJT is Open Source */
/*      software, licensed under the GNU General Public License V2 (GPL). */
/*      Source code and programming information may be found at */
/*      http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int xcor_(real *s2, integer *ipk, integer *nsteps, integer *
	nsym, integer *lag1, integer *lag2, real *ccf, real *ccf0, integer *
	lagpk, real *flip, real *fdot)
{
    /* Initialized data */

    static integer lagmin = 0;

    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    integer i_nint(real *);

    /* Local variables */
    static real a[320];
    static integer i__, j;
    static real x, a2[320], df;
    static integer ii;
    static real fac;
    static integer lag;
    static real rms, base, abot, clip, alow, atop, ahigh, ccfmin, ccfmax;
    extern /* Subroutine */ int pctile_(real *, real *, integer *, integer *, 
	    real *);
    static real dtstep;

/*     Computes ccf of a row of s2 and the pseudo-random array pr. */
/*     Returns peak of the CCF and the lag at which peak occurs.  For */
/*     JT65, the CCF peak may be either positive or negative, with */
/*     negative implying the "OOO" message. */
/* Max length of power spectra */
/* Max number of half-symbol steps */
/* 2d spectrum, stepped by half-symbols */
    /* Parameter adjustments */
    ccf -= -5;
    s2 -= 1025;

    /* Function Body */
/* Silence g77 warning */
    df = 2.691650390625f;
    dtstep = .5f / df;
    fac = dtstep / (df * 60.f);
    i__1 = *nsteps;
    for (j = 1; j <= i__1; ++j) {
	r__1 = (j - *nsteps / 2) * *fdot * fac;
	ii = i_nint(&r__1) + *ipk;
	a[j - 1] = s2[ii + (j << 10)];
    }
/*     If requested, clip the spectrum that will be cross correlated. */
    clipcom_1.nclip = 0;
/* Turn it off */
    if (clipcom_1.nclip > 0) {
	pctile_(a, a2, nsteps, &c__50, &base);
	r__1 = *nsteps * .16f;
	alow = a2[i_nint(&r__1) - 1];
	r__1 = *nsteps * .84f;
	ahigh = a2[i_nint(&r__1) - 1];
/* Computing MIN */
	r__1 = base - alow, r__2 = ahigh - base;
	rms = dmin(r__1,r__2);
	clip = 4.f - clipcom_1.nclip;
	atop = base + clip * rms;
	abot = base - clip * rms;
	i__1 = *nsteps;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (clipcom_1.nclip < 4) {
/* Computing MIN */
		r__1 = a[i__ - 1];
		a[i__ - 1] = dmin(r__1,atop);
/* Computing MAX */
		r__1 = a[i__ - 1];
		a[i__ - 1] = dmax(r__1,abot);
	    } else {
		if (a[i__ - 1] >= base) {
		    a[i__ - 1] = 1.f;
		} else {
		    a[i__ - 1] = -1.f;
		}
	    }
	}
    }
    ccfmax = 0.f;
    ccfmin = 0.f;
    i__1 = *lag2;
    for (lag = *lag1; lag <= i__1; ++lag) {
	x = 0.f;
	i__2 = *nsym;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    j = (i__ << 1) - 1 + lag;
	    if (j >= 1 && j <= *nsteps) {
		x += a[j - 1] * prcom_1.pr[i__ - 1];
	    }
	}
	ccf[lag] = x * 2;
/* The 2 is for plotting scale */
	if (ccf[lag] > ccfmax) {
	    ccfmax = ccf[lag];
	    *lagpk = lag;
	}
	if (ccf[lag] < ccfmin) {
	    ccfmin = ccf[lag];
	    lagmin = lag;
	}
    }
    *ccf0 = ccfmax;
    *flip = 1.f;
    if (-ccfmin > ccfmax) {
	i__1 = *lag2;
	for (lag = *lag1; lag <= i__1; ++lag) {
	    ccf[lag] = -ccf[lag];
	}
	*lagpk = lagmin;
	*ccf0 = -ccfmin;
	*flip = -1.f;
    }
    return 0;
} /* xcor_ */

