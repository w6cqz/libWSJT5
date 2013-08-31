/* fil653.f -- translated by f2c (version 20100827).
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
/* Subroutine */ int fil653_(complex *c1, integer *n1, complex *c2, integer *
	n2)
{
    /* Initialized data */

    static real a[45] = { -5.569862e-6f,-.002503777832f,-.004040335617f,
	    -.005717910288f,-.006153385485f,-.004446125293f,-3.05215272e-4f,
	    .005557289511f,.011329120672f,.01449655128f,.012703875898f,
	    .004837591829f,-.008060363689f,-.022474422302f,-.032964876083f,
	    -.033575486327f,-.019743889907f,.00989567234f,.052467109908f,
	    .101031155027f,.146073001698f,.177927966814f,.189427119395f,
	    .177927966814f,.146073001698f,.101031155027f,.052467109908f,
	    .00989567234f,-.019743889907f,-.033575486327f,-.032964876083f,
	    -.022474422302f,-.008060363689f,.004837591829f,.012703875898f,
	    .01449655128f,.011329120672f,.005557289511f,-3.05215272e-4f,
	    -.004446125293f,-.006153385485f,-.005717910288f,-.004040335617f,
	    -.002503777832f,-5.569862e-6f };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    complex q__1, q__2;

    /* Local variables */
    static integer i__, j, k, k0;

/*     FIR lowpass filter designed using ScopeFIR */
/*     fsample     = 1378.125   Hz */
/*     Ntaps       = 45 */
/*     fc          = 100        Hz */
/*     fstop       = 172.265625 Hz */
/*     Ripple      = 0.5        dB */
/*     Stop Atten  = 50         dB */
/*     fout        = 172.265625 Hz */
/*     BW          = 200        Hz */
/* Downsample ratio */
/*     Filter coefficients: */
    /* Parameter adjustments */
    --c1;
    --c2;

    /* Function Body */
    *n2 = (*n1 - 45 + 4) / 4;
    k0 = 19;
/*     Loop over all output samples */
    i__1 = *n2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	c2[i__2].r = 0.f, c2[i__2].i = 0.f;
	k = k0 + (i__ << 2);
	for (j = -22; j <= 22; ++j) {
	    i__2 = i__;
	    i__3 = i__;
	    i__4 = j + k;
	    i__5 = j + 22;
	    q__2.r = a[i__5] * c1[i__4].r, q__2.i = a[i__5] * c1[i__4].i;
	    q__1.r = c2[i__3].r + q__2.r, q__1.i = c2[i__3].i + q__2.i;
	    c2[i__2].r = q__1.r, c2[i__2].i = q__1.i;
	}
    }
    return 0;
} /* fil653_ */

