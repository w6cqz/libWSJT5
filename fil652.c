/* fil652.f -- translated by f2c (version 20100827).
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
/* Subroutine */ int fil652_(complex *c1, integer *n1, complex *c2, integer *
	n2)
{
    /* Initialized data */

    static real a[31] = { -8.59869246e-4f,-.008518289484f,-.011250152625f,
	    -.001061705256f,.013958392156f,.010047338728f,-.015003870003f,
	    -.025027880982f,.007151700557f,.043634723913f,.016788108012f,
	    -.061886192062f,-.07354845952f,.075261027466f,.306638863577f,
	    .419826269508f,.306638863577f,.075261027466f,-.07354845952f,
	    -.061886192062f,.016788108012f,.043634723913f,.007151700557f,
	    -.025027880982f,-.015003870003f,.010047338728f,.013958392156f,
	    -.001061705256f,-.011250152625f,-.008518289484f,-8.59869246e-4f };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    complex q__1, q__2;

    /* Local variables */
    static integer i__, j, k, k0;

/*  FIR lowpass filter designed using ScopeFIR */
/*  fsample     = 2756.25  Hz */
/*  Ntaps       = 31 */
/*  fc          = 500      Hz */
/*  fstop       = 689.0625 Hz */
/*  Ripple      = 0.5      dB */
/*  Stop Atten  = 50       dB */
/*  fout        = 1378.125 Hz */
/* Downsample ratio */
/*  Filter coefficients: */
    /* Parameter adjustments */
    --c1;
    --c2;

    /* Function Body */
    *n2 = (*n1 - 31 + 2) / 2;
    k0 = 14;
/*  Loop over all output samples */
    i__1 = *n2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	c2[i__2].r = 0.f, c2[i__2].i = 0.f;
	k = k0 + (i__ << 1);
	for (j = -15; j <= 15; ++j) {
	    i__2 = i__;
	    i__3 = i__;
	    i__4 = j + k;
	    i__5 = j + 15;
	    q__2.r = a[i__5] * c1[i__4].r, q__2.i = a[i__5] * c1[i__4].i;
	    q__1.r = c2[i__3].r + q__2.r, q__1.i = c2[i__3].i + q__2.i;
	    c2[i__2].r = q__1.r, c2[i__2].i = q__1.i;
	}
    }
    return 0;
} /* fil652_ */

