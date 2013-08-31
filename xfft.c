/* xfft.f -- translated by f2c (version 20100827).
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

static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__0 = 0;

/*      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*      contributions from additional authors.  WSJT is Open Source */
/*      software, licensed under the GNU General Public License V2 (GPL). */
/*      Source code and programming information may be found at */
/*      http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int xfft_(real *x, integer *nfft, integer *ical, real *
	wisfile)
{
    extern /* Subroutine */ int four2a_(real *, integer *, integer *, integer 
	    *, integer *, integer *, real *);

/*  Real-to-complex FFT. */
/*      call four2(x,nfft,1,-1,0) */
/*      print *,'Entering xfft(x,nfft,ical)' */
/*      print *,'call four2a(x,nfft,1,-1,0,ical)' */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    four2a_(&x[1], nfft, &c__1, &c_n1, &c__0, ical, wisfile);
/*      print *,'Done...' */
/*      print *,'Leaving xfft(x,nfft,ical)' */
    return 0;
} /* xfft_ */

