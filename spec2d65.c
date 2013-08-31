/* spec2d65.f -- translated by f2c (version 20100827).
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

/* Table of constant values */

static integer c__77 = 77;
static integer c__1 = 1;
static integer c_n1 = -1;

/*     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*     contributions from additional authors.  WSJT is Open Source */
/*     software, licensed under the GNU General Public License V2 (GPL). */
/*     Source code and programming information may be found at */
/*     http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int spec2d65_(real *dat, integer *jz, integer *nsym, real *
	flip, integer *istart, real *f0, real *ftrack, integer *nafc, real *
	s2, integer *ical, char *wisfile, ftnlen wisfile_len)
{
    /* Initialized data */

    static doublereal twopi = 6.28318530718;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real r__1, r__2;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;
    static complex equiv_0[2048];

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), r_imag(complex *);

    /* Local variables */
    static integer i__, j, k;
    static real s[77];
#define x ((real *)equiv_0)
    static real df, dt;
#define cx (equiv_0)
    static real ps[77];
    extern /* Subroutine */ int add_(real *, real *, real *, integer *);
    static real fac;
    static doublereal pha;
    static real ref[77], base;
    static doublereal dpha;
    static integer nref, nfft;
    extern /* Subroutine */ int move_(real *, real *, integer *), zero_(real *
	    , integer *), four2a_(complex *, integer *, integer *, integer *, 
	    integer *, integer *, char *, ftnlen), ftpeak65_(real *, integer *
	    , integer *, real *, real *, real *, integer *, real *);

/*     Computes the spectrum for each of 126 symbols. */
/*     NB: At this point, istart, f0, and ftrack are supposedly known. */
/*     The JT65 signal has Sync bin + 2 guard bins + 64 data bins = 67 */
/*     bins.  We add 5 extra bins at top and bottom for drift, making 77 */
/*     bins in all. */
/* Max length of FFTs */
/* Raw data */
/* Spectra of all symbols */
/*      complex work(NMAX) */
    /* Parameter adjustments */
    --dat;
    --ftrack;
    s2 -= 78;

    /* Function Body */
/*     Peak up in frequency and time, and compute ftrack. */
    ftpeak65_(&dat[1], jz, istart, f0, flip, prcom_1.pr, nafc, &ftrack[1]);
    nfft = 2048;
/* Size of FFTs */
    dt = 1.8140589569160998e-4f;
    df = 5512.5f / nfft;
    zero_(ps, &c__77);
    k = *istart - nfft;
/*     NB: this could be done starting with array c3, in ftpeak65, */
/*     instead of the dat() array.  Would save some time this way ... */
    i__1 = *nsym;
    for (j = 1; j <= i__1; ++j) {
	zero_(s, &c__77);
	k += nfft;
	if (k >= 1 && k <= *jz - nfft) {
/*           Mix sync tone down to f=5*df (==> bin 6 of array cx, after FFT) */
	    dpha = twopi * dt * (*f0 + ftrack[j] - df * 5.f);
	    pha = 0.f;
	    i__2 = nfft;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		pha += dpha;
		i__3 = i__ - 1;
		i__4 = k - 1 + i__;
		d__1 = cos(pha);
		d__2 = -sin(pha);
		z__2.r = d__1, z__2.i = d__2;
		z__1.r = dat[i__4] * z__2.r, z__1.i = dat[i__4] * z__2.i;
		cx[i__3].r = z__1.r, cx[i__3].i = z__1.i;
	    }
	    four2a_(cx, &nfft, &c__1, &c_n1, &c__1, ical, wisfile, (ftnlen)
		    255);
	    for (i__ = 1; i__ <= 77; ++i__) {
		i__2 = i__ - 1;
/* Computing 2nd power */
		r__1 = cx[i__2].r;
/* Computing 2nd power */
		r__2 = r_imag(&cx[i__ - 1]);
		s[i__ - 1] = s[i__ - 1] + r__1 * r__1 + r__2 * r__2;
	    }
	} else {
	    zero_(s, &c__77);
	}
	move_(s, &s2[j * 77 + 1], &c__77);
	add_(ps, s, ps, &c__77);
    }
/*     Flatten the spectra by dividing through by the average of the */
/*     "sync on" spectra, with the sync tone explicitly deleted. */
    nref = *nsym / 2;
    for (i__ = 1; i__ <= 77; ++i__) {
/*     First we sum all the sync-on spectra: */
	ref[i__ - 1] = 0.f;
	i__1 = *nsym;
	for (j = 1; j <= i__1; ++j) {
	    if (*flip * prcom_1.pr[j - 1] > 0.f) {
		ref[i__ - 1] += s2[i__ + j * 77];
	    }
	}
	ref[i__ - 1] /= nref;
/* Normalize */
    }
/*     Remove the sync tone itself: */
    base = (ref[0] + ref[1] + ref[9] + ref[10]) * .25f;
    for (i__ = 3; i__ <= 9; ++i__) {
	ref[i__ - 1] = base;
    }
/*     Now flatten the spectra for all the data symbols: */
    for (i__ = 1; i__ <= 77; ++i__) {
	fac = 1.f / ref[i__ - 1];
	i__1 = *nsym;
	for (j = 1; j <= i__1; ++j) {
	    s2[i__ + j * 77] = fac * s2[i__ + j * 77];
	    if (s2[i__ + j * 77] == 0.f) {
		s2[i__ + j * 77] = 1.f;
	    }
/* To fix problem in mfskprob */
	}
    }
    return 0;
} /* spec2d65_ */

#undef cx
#undef x


