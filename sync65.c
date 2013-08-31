/* sync65.f -- translated by f2c (version 20100827).
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

static integer c__1024 = 1024;
static integer c__320 = 320;
static integer c__745 = 745;
static real c_b5 = 0.f;
static integer c__45 = 45;

/*      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*      contributions from additional authors.  WSJT is Open Source */
/*      software, licensed under the GNU General Public License V2 (GPL). */
/*      Source code and programming information may be found at */
/*      http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int sync65_(real *dat, integer *jz, integer *dftolerance, 
	integer *nfreeze, integer *mousedf, real *dtx, real *dfx, real *snrx, 
	real *snrsync, real *ccfblue, real *ccfred1, real *flip, real *width, 
	integer *ical, char *wisfile, ftnlen wisfile_len)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    integer i_nint(real *);
    double sqrt(doublereal);

    /* Local variables */
    static real syncbest;
    static integer i__, j, k;
    static real syncbest2;
    static integer i0;
    static real s2[327680]	/* was [1024][320] */, x1, x2;
    extern doublereal db_(real *);
    static real fa, fb, df;
    static integer ia, ib, nh;
    static real dt, dx;
    extern /* Subroutine */ int ps_(real *, integer *, real *, integer *, 
	    char *, ftnlen);
    static real sq, dx2;
    extern /* Subroutine */ int add_(real *, real *, real *, integer *);
    static integer lag, ipk, jpk;
    static real tmp[450];
    static integer nsq;
    static real rms, ccf0;
    static integer lag1, lag2, ipk2;
    static real base, xlag;
    static integer nfft;
    extern /* Subroutine */ int xcor_(real *, integer *, integer *, integer *,
	     integer *, integer *, real *, real *, integer *, real *, real *);
    static real sync;
    extern /* Subroutine */ int zero_(real *, integer *);
    static integer nsym;
    extern /* Subroutine */ int flat1_(real *, real *, integer *, integer *, 
	    integer *, integer *);
    static real famin, fbmax;
    static integer lagpk;
    extern /* Subroutine */ int limit_(real *, integer *);
    static real psavg[1024];
    extern /* Subroutine */ int slope_(real *, integer *, real *);
    static real ppmax, stest;
    static integer lagpk0, lagpk2;
    static real ccfred[901], ccfmax;
    extern /* Subroutine */ int pctile_(real *, real *, integer *, integer *, 
	    real *), peakup_(real *, real *, real *, real *);
    static integer istart, nsteps;

/*     Synchronizes JT65 data, finding the best-fit DT and DF. */
/*     NB: at this stage, submodes ABC are processed in the same way. */
/* Size of data array */
/* Max length of FFTs */
/* Max length of power spectra */
/* Max number of half-symbol steps */
/* Range of DF search */
/* Average spectrum of whole record */
/* 2d spectrum, stepped by half-symbols */
/*     The value 450 is empirical: */
/* CCF with pseudorandom sequence */
/* Peak of ccfblue, as function of freq */
/* Peak of ccfblue, as function of freq */
/*     Do FFTs of symbol length, stepped by half symbols.  Note that we */
/*     have already downsampled the data by factor of 2. */
    /* Parameter adjustments */
    --dat;
    ccfblue -= -5;
    ccfred1 -= -224;

    /* Function Body */
    nsym = 126;
    nfft = 2048;
    nsteps = (*jz << 1) / nfft - 1;
    nh = nfft / 2;
    df = 5512.5f / nfft;
/*     Compute power spectrum for each step and get average */
    zero_(psavg, &nh);
    i__1 = nsteps;
    for (j = 1; j <= i__1; ++j) {
	k = (j - 1) * nh + 1;
	limit_(&dat[k], &nfft);
	ps_(&dat[k], &nfft, &s2[(j << 10) - 1024], ical, wisfile, (ftnlen)255)
		;
	add_(psavg, &s2[(j << 10) - 1024], psavg, &nh);
    }
    flat1_(psavg, s2, &nh, &nsteps, &c__1024, &c__320);
/*     Find the best frequency channel for CCF */
/*      famin= 670.46 */
/*      fbmax=1870.46 */
/* Flatten the spe */
    famin = 3.f;
    fbmax = 2700.f;
    fa = famin;
    fb = fbmax;
/*     [DEBUG] Implicitly setting NFreeze */
    *nfreeze = 1;
    if (*nfreeze == 1) {
/* Computing MAX */
	r__1 = famin, r__2 = *mousedf + 1270.46f - *dftolerance;
	fa = dmax(r__1,r__2);
/* Computing MIN */
	r__1 = fbmax, r__2 = *mousedf + 1270.46f + *dftolerance;
	fb = dmin(r__1,r__2);
    } else {
/* Computing MAX */
	r__1 = famin, r__2 = *mousedf + 1270.46f - 600;
	fa = dmax(r__1,r__2);
/* Computing MIN */
	r__1 = fbmax, r__2 = *mousedf + 1270.46f + 600;
	fb = dmin(r__1,r__2);
    }
    ia = fa / df;
    ib = fb / df;
    r__1 = 1270.46f / df;
    i0 = i_nint(&r__1);
    lag1 = -5;
    lag2 = 59;
    syncbest = -1e30f;
    syncbest2 = -1e30f;
    zero_(ccfred, &c__745);
    i__1 = ib;
    for (i__ = ia; i__ <= i__1; ++i__) {
	xcor_(s2, &i__, &nsteps, &nsym, &lag1, &lag2, &ccfblue[-5], &ccf0, &
		lagpk0, flip, &c_b5);
	j = i__ - i0;
	if (j >= -372 && j <= 372) {
	    ccfred[j + 450] = ccf0;
	}
/*     Find rms of the CCF, without the main peak */
	i__2 = lag2 - lag1 + 1;
	r__1 = lagpk0 - lag1 + 1.f;
	slope_(&ccfblue[lag1], &i__2, &r__1);
	sync = (r__1 = ccfblue[lagpk0], dabs(r__1));
	ppmax = psavg[i__ - 1] - 1.f;
/*     Find the best sync value */
	if (sync > syncbest2) {
	    ipk2 = i__;
	    lagpk2 = lagpk0;
	    syncbest2 = sync;
	}
/*     We are most interested if snrx will be more than -30 dB. */
	if (ppmax > .2938f) {
/* Corresponds to snrx.gt.-30.0 */
	    if (sync > syncbest) {
		ipk = i__;
		lagpk = lagpk0;
		syncbest = sync;
	    }
	}
    }
/*     If we found nothing with snrx > -30 dB, take the best sync that */
/*     *was* found. */
    if (syncbest < -10.f) {
	ipk = ipk2;
	lagpk = lagpk2;
	syncbest = syncbest2;
    }
/*     Peak up in frequency to fraction of channel */
    base = (psavg[ipk - 4] + psavg[ipk - 3] + psavg[ipk + 1] + psavg[ipk + 2])
	     * .25f;
/*      call peakup(psavg(ipk-1),psavg(ipk),psavg(ipk+1),dx) */
/*      if(dx.lt.-1.0) dx=-1.0 */
/*      if(dx.gt.1.0) dx=1.0 */
    dx = 0.f;
    *dfx = (ipk + dx - i0) * df;
/*     Peak up in time, at best whole-channel frequency */
    xcor_(s2, &ipk, &nsteps, &nsym, &lag1, &lag2, &ccfblue[-5], &ccfmax, &
	    lagpk, flip, &c_b5);
    xlag = (real) lagpk;
    if (lagpk > lag1 && lagpk < lag2) {
	peakup_(&ccfblue[lagpk - 1], &ccfmax, &ccfblue[lagpk + 1], &dx2);
	xlag = lagpk + dx2;
    }
/*     Find rms of the CCF, without the main peak */
    i__1 = lag2 - lag1 + 1;
    r__1 = xlag - lag1 + 1.f;
    slope_(&ccfblue[lag1], &i__1, &r__1);
    sq = 0.f;
    nsq = 0;
    i__1 = lag2;
    for (lag = lag1; lag <= i__1; ++lag) {
	if ((r__1 = lag - xlag, dabs(r__1)) > 2.f) {
/* Computing 2nd power */
	    r__1 = ccfblue[lag];
	    sq += r__1 * r__1;
	    ++nsq;
	}
    }
    rms = sqrt(sq / nsq);
    *snrsync = (r__1 = ccfblue[lagpk], dabs(r__1)) / rms - 1.1f;
/* Empirical */
    dt = 1.8140589569160998e-4f;
    istart = xlag * nh;
    *dtx = istart * dt;
    *snrx = -99.f;
/*      ppmax=psavg(ipk)/base-1.0 */
    ppmax = psavg[ipk - 1] - 1.f;
/*     Plus 3 dB because sync tone is on half the time.  (Don't */
/*     understand why an additional +2 dB is needed ...) */
    if (ppmax > 1e-4f) {
	r__1 = ppmax * df / 2500.f;
	*snrx = db_(&r__1) + 5.f;
    }
/* ### */
    if (*snrx < -33.f) {
	*snrx = -33.f;
    }
/*     Compute width of sync tone to outermost -3 dB points */
    i__1 = ib - ia + 1;
    pctile_(&ccfred[ia - i0 + 450], tmp, &i__1, &c__45, &base);
    jpk = ipk - i0;
    stest = base + (ccfred[jpk + 450] - base) * .5f;
/* -3 dB */
    for (i__ = -10; i__ <= 0; ++i__) {
	if (jpk + i__ >= -371) {
	    if (ccfred[jpk + i__ + 450] > stest) {
		goto L30;
	    }
	}
    }
    i__ = 0;
L30:
    x1 = i__ - 1 + (stest - ccfred[jpk + i__ + 449]) / (ccfred[jpk + i__ + 
	    450] - ccfred[jpk + i__ + 449]);
    for (i__ = 10; i__ >= 0; --i__) {
	if (jpk + i__ <= 371) {
	    if (ccfred[jpk + i__ + 450] > stest) {
		goto L32;
	    }
	}
    }
    i__ = 0;
L32:
    x2 = i__ + 1 - (stest - ccfred[jpk + i__ + 451]) / (ccfred[jpk + i__ + 
	    450] - ccfred[jpk + i__ + 451]);
    *width = x2 - x1;
    if (*width > 1.2f) {
/* Computing 2nd power */
	r__1 = *width;
	*width = sqrt(r__1 * r__1 - 1.44f);
    }
    *width = df * *width;
/* Computing MAX */
    r__1 = 0.f, r__2 = dmin(99.f,*width);
    *width = dmax(r__1,r__2);
    for (i__ = -224; i__ <= 224; ++i__) {
	ccfred1[i__] = ccfred[i__ + 450];
    }
    return 0;
} /* sync65_ */

