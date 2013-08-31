/* fil651.f -- translated by f2c (version 20100827).
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
/* Subroutine */ int fil651_(real *d__, integer *n1, complex *c__, integer *
	n2)
{
    /* Initialized data */

    static complex ck[31] = { {-7.3578e-8f,-8.59869243e-4f},{.008518289457f,
	    -6.80308e-7f},{8.34309e-7f,.011250152594f},{-.001061705254f,
	    7.2679e-8f},{8.75897e-7f,.013958392128f},{-.010047338711f,
	    5.7316e-7f},{7.7032e-7f,.015003869984f},{-.025027880956f,
	    1.142192e-6f},{2.85583e-7f,.007151700551f},{-.043634723888f,
	    1.493512e-6f},{-4.78847e-7f,-.016788108005f},{-.061886192046f,
	    1.412144e-6f},{-1.258694e-6f,-.073548459509f},{-.075261027462f,
	    8.58668e-7f},{-1.749252e-6f,-.306638863572f},{.419826269508f,0.f},
	    {-1.749252e-6f,.306638863572f},{-.075261027462f,-8.58668e-7f},{
	    -1.258694e-6f,.073548459509f},{-.061886192046f,-1.412144e-6f},{
	    -4.78847e-7f,.016788108005f},{-.043634723888f,-1.493512e-6f},{
	    2.85583e-7f,-.007151700551f},{-.025027880956f,-1.142192e-6f},{
	    7.7032e-7f,-.015003869984f},{-.010047338711f,-5.7316e-7f},{
	    8.75897e-7f,-.013958392128f},{-.001061705254f,-7.2679e-8f},{
	    8.34309e-7f,-.011250152594f},{.008518289457f,6.80308e-7f},{
	    -7.3578e-8f,8.59869243e-4f} };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    void r_cnjg(complex *, complex *);

    /* Local variables */
    static integer i__, j, k, k0;

/*  FIR lowpass mixing filter designed with ScopeFIR.  Real in, complex out. */
/*  fsample     = 5512.5   Hz */
/*  Ntaps       = 31 */
/*  fc          = 1000     Hz */
/*  fstop       = 1378.125 Hz */
/*  Ripple      = 0.5      dB */
/*  Stop Atten  = 50       dB */
/*  fmix        = 1378.125 Hz */
/*  fout        = 2706.25  Hz */
/* Downsample ratio */
    /* Parameter adjustments */
    --d__;
    --c__;

    /* Function Body */
    *n2 = (*n1 - 31 + 2) / 2;
    k0 = 14;
    i__1 = *n2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	c__[i__2].r = 0.f, c__[i__2].i = 0.f;
	k = k0 + (i__ << 1);
	for (j = -15; j <= 15; ++j) {
	    i__2 = i__;
	    i__3 = i__;
	    i__4 = j + k;
	    r_cnjg(&q__3, &ck[j + 15]);
	    q__2.r = d__[i__4] * q__3.r, q__2.i = d__[i__4] * q__3.i;
	    q__1.r = c__[i__3].r + q__2.r, q__1.i = c__[i__3].i + q__2.i;
	    c__[i__2].r = q__1.r, c__[i__2].i = q__1.i;
	}
    }
    return 0;
} /* fil651_ */

