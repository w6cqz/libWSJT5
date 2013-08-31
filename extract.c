/* extract.f -- translated by f2c (version 20100827).
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
    integer ntdecode;
} extcom_;

#define extcom_1 extcom_

/* Table of constant values */

static integer c__63 = 63;
static integer c_n1 = -1;

/* Subroutine */ int extract_(real *s3, integer *nadd, integer *ncount, 
	integer *mrsym, integer *mr2sym, integer *mrprob, integer *mr2prob)
{

    extern /* Subroutine */ int graycode_(integer *, integer *, integer *);
    static integer i__;
    extern /* Subroutine */ int interleave63_(integer *, integer *);
    static integer nlow, ntest;
    extern /* Subroutine */ int demod64a_(real *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *);

    /* Parameter adjustments */
    --mr2prob;
    --mrprob;
    --mr2sym;
    --mrsym;
    s3 -= 65;

    /* Function Body */
/*     Clearing the decoded symbol/probability arrays. */
    for (i__ = 1; i__ <= 63; ++i__) {
	mrsym[i__] = 0;
	mr2sym[i__] = 0;
	mrprob[i__] = 0;
	mr2prob[i__] = 0;
    }
/* L1: */
    demod64a_(&s3[65], nadd, &mrsym[1], &mrprob[1], &mr2sym[1], &mr2prob[1], &
	    ntest, &nlow);
    if (ntest < 50 || nlow > 20) {
	*ncount = -999;
/* Flag bad data */
	goto L900;
    }
/*     Most probable decoded symbols */
    graycode_(&mrsym[1], &c__63, &c_n1);
/* Removes Gray coding */
    interleave63_(&mrsym[1], &c_n1);
/* Deinterleaves symbols */
    interleave63_(&mrprob[1], &c_n1);
/*     Next most probable decoded symbols */
/* Deinterleaves probabilites */
    graycode_(&mr2sym[1], &c__63, &c_n1);
    interleave63_(&mr2sym[1], &c_n1);
    interleave63_(&mr2prob[1], &c_n1);
L900:
    return 0;
} /* extract_ */

