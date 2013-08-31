/* interleave63.f -- translated by f2c (version 20100827).
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

static integer c__63 = 63;

/*     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*     contributions from additional authors.  WSJT is Open Source */
/*     software, licensed under the GNU General Public License V2 (GPL). */
/*     Source code and programming information may be found at */
/*     http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int interleave63_(integer *d1, integer *idir)
{
    static integer i__, j, d2[63]	/* was [9][7] */;
    extern /* Subroutine */ int move_(integer *, integer *, integer *);

/*     Interleave (idir=1) or de-interleave (idir=-1) the array d1. */
    if (*idir >= 0) {
	for (i__ = 0; i__ <= 6; ++i__) {
	    for (j = 0; j <= 8; ++j) {
		d2[j + i__ * 9] = d1[i__ + j * 7];
	    }
	}
	move_(d2, d1, &c__63);
    } else {
	move_(d1, d2, &c__63);
	for (i__ = 0; i__ <= 6; ++i__) {
	    for (j = 0; j <= 8; ++j) {
		d1[i__ + j * 7] = d2[j + i__ * 9];
	    }
	}
    }
    return 0;
} /* interleave63_ */

