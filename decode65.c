/* decode65.f -- translated by f2c (version 20100827).
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
    real ppsave[483840]	/* was [64][63][120] */;
    integer nflag[120], nsave, iseg[120];
} ave_;

#define ave_1 ave_

struct {
    real pr[135];
    integer mdat[126], mref[252]	/* was [126][2] */, mdat2[126], mref2[
	    252]	/* was [126][2] */;
} prcom_;

#define prcom_1 prcom_

/*     Modified by W6CQZ (c) 2012 */
/*     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*     contributions from additional authors.  WSJT is Open Source */
/*     software, licensed under the GNU General Public License V2 (GPL). */
/*     Source code and programming information may be found at */
/*     http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int decode65_(real *dat, integer *npts, real *dtx, real *dfx,
	 real *flip, integer *ncount, integer *ical, char *wisfile, real *
	dsym1, real *dsym2, real *dsym1p, real *dsym2p, ftnlen wisfile_len)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    integer i_nint(real *);

    /* Local variables */
    static integer i__, j, k;
    static real f0, s2[9702]	/* was [77][126] */, s3[4032]	/* was [64][
	    63] */, dt;
    static integer nadd, nafc, nsym;
    static real ftrack[126];
    static integer istart;
    extern /* Subroutine */ int spec2d65_(real *, integer *, integer *, real *
	    , integer *, real *, real *, integer *, real *, integer *, char *,
	     ftnlen), extract_(real *, integer *, integer *, real *, real *, 
	    real *, real *);

/*     Decodes JT65 data, assuming that DT and DF have already been */
/*     determined. */
/* Raw data */
    /* Parameter adjustments */
    --dat;

    /* Function Body */
    nafc = 1;
    dt = 1.8140589569160998e-4f;
/* Sample interval (2x downsampled data) */
    r__1 = *dtx / dt;
    istart = i_nint(&r__1);
/* Start index for synced FFTs */
    nsym = 126;
/*     Compute spectra of the channel symbols */
    f0 = *dfx + 1270.46f;
    spec2d65_(&dat[1], npts, &nsym, flip, &istart, &f0, ftrack, &nafc, s2, 
	    ical, wisfile, (ftnlen)255);
    for (j = 1; j <= 63; ++j) {
	k = prcom_1.mdat[j - 1];
/* Points to data symbol */
	if (*flip < 0.f) {
	    k = prcom_1.mdat2[j - 1];
	}
	for (i__ = 1; i__ <= 64; ++i__) {
	    s3[i__ + (j << 6) - 65] = s2[i__ + 7 + k * 77 - 78];
	}
    }
    nadd = 1;
    extract_(s3, &nadd, ncount, dsym1, dsym2, dsym1p, dsym2p);
    return 0;
} /* decode65_ */

