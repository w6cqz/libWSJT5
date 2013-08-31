/* demod64a.f -- translated by f2c (version 20100827).
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

static doublereal c_b2 = .64;

/*     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*     contributions from additional authors.  WSJT is Open Source */
/*     software, licensed under the GNU General Public License V2 (GPL). */
/*     Source code and programming information may be found at */
/*     http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int demod64a_(real *signal, integer *nadd, integer *mrsym, 
	integer *mrprob, integer *mr2sym, integer *mr2prob, integer *ntest, 
	integer *nlow)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), exp(doublereal);

    /* Local variables */
    static integer i__, j;
    static doublereal x;
    static integer i1, i2;
    static doublereal p1, p2;
    static integer s1;
    static doublereal s2, fs[64], ave;
    static integer mrs[63];
    static doublereal sum;
    static integer mrs2[63];
    static doublereal afac, fsum, scale;

/*     Demodulate the 64-bin spectra for each of 63 symbols in a frame. */
/*     Parameters */
/*     nadd     number of spectra already summed */
/*     mrsym    most reliable symbol value */
/*     mr2sym   second most likely symbol value */
/*     mrprob   probability that mrsym was the transmitted value 0...255 */
/*     mr2prob  probability that mr2sym was the transmitted value */
/*      implicit real*8 (a-h,o-z) */
/*      common/tmp9/ mrs(63),mrs2(63) */
    /* Parameter adjustments */
    --mr2prob;
    --mr2sym;
    --mrprob;
    --mrsym;
    signal -= 65;

    /* Function Body */
    d__1 = (doublereal) ((real) (*nadd));
    afac = pow_dd(&d__1, &c_b2) * 1.1f;
    scale = 255.999f;
/*     Compute average spectral value */
    sum = 0.f;
    for (j = 1; j <= 63; ++j) {
	for (i__ = 1; i__ <= 64; ++i__) {
	    sum += signal[i__ + (j << 6)];
	}
    }
    ave = sum / 4032.f;
/*     Compute probabilities for most reliable symbol values */
    for (j = 1; j <= 63; ++j) {
	s1 = -1e30f;
	fsum = 0.f;
	i1 = 0;
/* Shut up compiler warnings. -db */
	for (i__ = 1; i__ <= 64; ++i__) {
/* Computing MIN */
	    d__1 = afac * signal[i__ + (j << 6)] / ave;
	    x = min(d__1,50.);
	    fs[i__ - 1] = exp(x);
	    fsum += fs[i__ - 1];
	    if (signal[i__ + (j << 6)] > (real) s1) {
		s1 = signal[i__ + (j << 6)];
		i1 = i__;
/* Most reliable */
	    }
	}
	s2 = -1e30f;
	i2 = 0;
/* Shut up compiler warnings. -db */
	for (i__ = 1; i__ <= 64; ++i__) {
	    if (i__ != i1 && signal[i__ + (j << 6)] > s2) {
		s2 = signal[i__ + (j << 6)];
		i2 = i__;
/* Second most reliable */
	    }
	}
	p1 = fs[i1 - 1] / fsum;
/* Normalized probabilities */
	p2 = fs[i2 - 1] / fsum;
	mrsym[j] = i1 - 1;
	mr2sym[j] = i2 - 1;
	mrprob[j] = (integer) (scale * p1);
	mr2prob[j] = (integer) (scale * p2);
	mrs[j - 1] = i1;
	mrs2[j - 1] = i2;
    }
    sum = 0.f;
    *nlow = 0;
    for (j = 1; j <= 63; ++j) {
	sum += mrprob[j];
	if (mrprob[j] <= 5) {
	    ++(*nlow);
	}
    }
    *ntest = (integer) (sum / 63);
    return 0;
} /* demod64a_ */

