/* ftpeak65.f -- translated by f2c (version 20100827).
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

static integer c__16 = 16;

/*     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*     contributions from additional authors.  WSJT is Open Source */
/*     software, licensed under the GNU General Public License V2 (GPL). */
/*     Source code and programming information may be found at */
/*     http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int ftpeak65_(real *dat, integer *jz, integer *istart, real *
	f0, real *flip, real *pr, integer *nafc, real *ftrack)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real r__1;
    doublereal d__1, d__2;
    complex q__1, q__2;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double atan(doublereal), sin(doublereal), cos(doublereal);
    integer i_nint(real *);
    double r_imag(complex *);

    /* Local variables */
    static doublereal fsyncset;
    extern /* Subroutine */ int symsync65_(complex *, integer *, integer *, 
	    real *, real *, real *, integer *, integer *, real *, real *);
    static real c__[808]	/* was [101][8] */;
    static integer i__, j, k, n;
    static real s[5167];
    static complex z__, c2[165375], c3[82687], c4[20671], c5[5167], c6[5167];
    static integer k0, i0, n2, n3, n4, n5;
    static real df;
    static integer ia, ib;
    static doublereal dt;
    static integer iz;
    static real xj, ss, ccf[257];
    static integer idf;
    static doublereal pha;
    static integer ipk, kpk, jpk;
    static real sum;
    static doublereal dpha;
    static real fdot;
    static integer jmax;
    static real smax;
    extern /* Subroutine */ int fil651_(real *, integer *, complex *, integer 
	    *), fil652_(complex *, integer *, complex *, integer *), fil653_(
	    complex *, integer *, complex *, integer *);
    static real dfreq, ssmax;
    static doublereal twopi;
    static integer idfmax;

/*     Do the the JT65 "peakup" procedure in frequency and time; then */
/*     compute ftrack. */
    /* Parameter adjustments */
    --dat;
    --pr;
    --ftrack;

    /* Function Body */
    twopi = atan(1.) * 8;
    fsyncset = -300.;
    dt = 1.8140589569160998e-4;
/* Input dt (WSJT has downsampled by 2) */
    n2 = 165375;
    fil651_(&dat[1], jz, c2, &n2);
/* Filter and complex mix; rate 1/2 */
    dt *= 2.;
/* We're now downsampled by 4 */
    dpha = twopi * dt * (*f0 - fsyncset);
/* Put sync tone at fsyncset */
    pha = 0.f;
    i__1 = n2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	pha += dpha;
	i__2 = i__ - 1;
	i__3 = i__ - 1;
	d__1 = cos(pha);
	d__2 = -sin(pha);
	z__2.r = d__1, z__2.i = d__2;
	z__1.r = c2[i__3].r * z__2.r - c2[i__3].i * z__2.i, z__1.i = c2[i__3]
		.r * z__2.i + c2[i__3].i * z__2.r;
	c2[i__2].r = z__1.r, c2[i__2].i = z__1.i;
    }
    n3 = 82687;
    fil652_(c2, &n2, c3, &n3);
/* Low-pass at +/- 500 Hz; rate 1/2 */
    dt *= 2.;
/* Down by 8 */
    dpha = twopi * dt * fsyncset;
/* Mix sync tone to f=0 */
    pha = 0.f;
    i__1 = n3;
    for (i__ = 1; i__ <= i__1; ++i__) {
	pha += dpha;
	i__2 = i__ - 1;
	i__3 = i__ - 1;
	d__1 = cos(pha);
	d__2 = -sin(pha);
	z__2.r = d__1, z__2.i = d__2;
	z__1.r = c3[i__3].r * z__2.r - c3[i__3].i * z__2.i, z__1.i = c3[i__3]
		.r * z__2.i + c3[i__3].i * z__2.r;
	c3[i__2].r = z__1.r, c3[i__2].i = z__1.i;
    }
    n4 = 20671;
    fil653_(c3, &n3, c4, &n4);
/* Low-pass at +/- 100 Hz; rate 1/4 */
    dt *= 4.;
/* Down by 32 */
    n5 = 5167;
    fil653_(c4, &n4, c5, &n5);
/* Low-pass at +/- 25 Hz; rate 1/4 */
    dt *= 4.;
/*     Use f0 and istart (as found by sync65) and do CCFs against the */
/*     pr(126) array to get improved symbol synchronization. */
/*     NB: if istart is increased by 64, kpk will decrease by 1. */
/* Down by 128 */
    r__1 = *istart / 64.f - 7.f;
    k0 = i_nint(&r__1);
    symsync65_(c5, &n5, &k0, s, flip, &pr[1], &c__16, &kpk, ccf, &smax);
/*     Fix up the value of istart.  (The -1 is empirical.) */
    *istart += (kpk - 1.f) * 64.f;
/*     OK, we have symbol synchronization.  Now find peak ccf value as a */
/*     function of DF, for each group of 16 symbols. */
/*     What about using filter fil657? */
    df = .67291259765625f;
/* Oversample to get accurate peak */
    idfmax = 50;
    iz = n5 - 31;
    i__1 = idfmax;
    for (idf = -idfmax; idf <= i__1; ++idf) {
	dpha = twopi * idf * df * dt;
	pha = 0.f;
	i__2 = iz;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    pha += dpha;
	    i__3 = i__ - 1;
	    i__4 = i__ - 1;
	    d__1 = cos(pha);
	    d__2 = -sin(pha);
	    z__2.r = d__1, z__2.i = d__2;
	    z__1.r = c5[i__4].r * z__2.r - c5[i__4].i * z__2.i, z__1.i = c5[
		    i__4].r * z__2.i + c5[i__4].i * z__2.r;
	    c6[i__3].r = z__1.r, c6[i__3].i = z__1.i;
	}
	z__.r = 0.f, z__.i = 0.f;
	for (i__ = 1; i__ <= 32; ++i__) {
	    i__2 = i__ - 1;
	    q__1.r = z__.r + c6[i__2].r, q__1.i = z__.i + c6[i__2].i;
	    z__.r = q__1.r, z__.i = q__1.i;
	}
	s[0] = z__.r * z__.r + r_imag(&z__) * r_imag(&z__);
	i__2 = n5;
	for (i__ = 33; i__ <= i__2; ++i__) {
	    i__3 = i__ - 1;
	    q__2.r = z__.r + c6[i__3].r, q__2.i = z__.i + c6[i__3].i;
	    i__4 = i__ - 33;
	    q__1.r = q__2.r - c6[i__4].r, q__1.i = q__2.i - c6[i__4].i;
	    z__.r = q__1.r, z__.i = q__1.i;
	    s[i__ - 32] = z__.r * z__.r + r_imag(&z__) * r_imag(&z__);
	}
	for (n = 1; n <= 8; ++n) {
	    r__1 = (n - 1) * 126.f / 8.f + 1.f;
	    ia = i_nint(&r__1);
	    ib = ia + 15;
	    sum = 0.f;
	    i__2 = ib;
	    for (i__ = ia; i__ <= i__2; ++i__) {
		j = (i__ - 1 << 5) + k0 + kpk;
		if (j >= 1 && j <= iz) {
		    sum += *flip * pr[i__] * s[j - 1];
		}
	    }
	    c__[idf + n * 101 - 51] = sum / smax;
	}
    }
/*     Get drift rate and compute ftrack. */
/*      call getfdot(c,nafc,ftrack) */
    jmax = 0;
    if (*nafc == 1) {
	jmax = 25;
    }
    ssmax = 0.f;
    jpk = 0;
/* Shut up compiler warnings. -db */
    ipk = 0;
/* Shut up compiler warnings. -db */
    i__1 = jmax;
    for (j = -jmax; j <= i__1; ++j) {
	for (i__ = -25; i__ <= 25; ++i__) {
	    ss = 0.f;
	    xj = j / 7.f;
	    for (n = 1; n <= 8; ++n) {
		r__1 = i__ + (n - 4.5f) * xj;
		k = i_nint(&r__1);
		ss += c__[k + n * 101 - 51];
	    }
	    if (ss > ssmax) {
		ssmax = ss;
		ipk = i__;
		jpk = j;
	    }
	}
    }
    df = .67291259765625f;
    dfreq = ipk * df;
    fdot = jpk * df * 60.f / 40.949999999999996f;
    for (i__ = 1; i__ <= 126; ++i__) {
	ftrack[i__] = dfreq + fdot * .77999999999999992f * (i__ - 63.5f) / 
		126.f;
    }
    pha = 0.f;
    i0 = k0 + kpk + 2000;
    i__1 = iz;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r__1 = (i__ - i0) / 32.f + 63.5f;
	k = i_nint(&r__1);
	if (k < 1) {
	    k = 1;
	}
	if (k > 126) {
	    k = 126;
	}
	dpha = twopi * dt * ftrack[k];
	pha += dpha;
	i__2 = i__ - 1;
	i__3 = i__ - 1;
	d__1 = cos(pha);
	d__2 = -sin(pha);
	z__2.r = d__1, z__2.i = d__2;
	z__1.r = c5[i__3].r * z__2.r - c5[i__3].i * z__2.i, z__1.i = c5[i__3]
		.r * z__2.i + c5[i__3].i * z__2.r;
	c6[i__2].r = z__1.r, c6[i__2].i = z__1.i;
    }
    z__.r = 0.f, z__.i = 0.f;
    for (i__ = 1; i__ <= 32; ++i__) {
	i__1 = i__ - 1;
	q__1.r = z__.r + c6[i__1].r, q__1.i = z__.i + c6[i__1].i;
	z__.r = q__1.r, z__.i = q__1.i;
    }
    s[0] = z__.r * z__.r + r_imag(&z__) * r_imag(&z__);
    i__1 = n5;
    for (i__ = 33; i__ <= i__1; ++i__) {
	i__2 = i__ - 1;
	q__2.r = z__.r + c6[i__2].r, q__2.i = z__.i + c6[i__2].i;
	i__3 = i__ - 33;
	q__1.r = q__2.r - c6[i__3].r, q__1.i = q__2.i - c6[i__3].i;
	z__.r = q__1.r, z__.i = q__1.i;
	s[i__ - 32] = z__.r * z__.r + r_imag(&z__) * r_imag(&z__);
    }
    sum = 0.f;
    for (i__ = 1; i__ <= 126; ++i__) {
	j = (i__ - 1 << 5) + k0 + kpk;
	if (j >= 1 && j <= iz) {
	    sum += *flip * pr[i__] * s[j - 1];
	}
    }
    return 0;
} /* ftpeak65_ */

