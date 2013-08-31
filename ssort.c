/* ssort.f -- translated by f2c (version 20100827).
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

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/*      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with */
/*      contributions from additional authors.  WSJT is Open Source */
/*      software, licensed under the GNU General Public License V2 (GPL). */
/*      Source code and programming information may be found at */
/*      http://developer.berlios.de/projects/wsjt/. */
/* Subroutine */ int ssort_(real *x, real *y, integer *n, integer *kflag)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer i__, j, k, l, m;
    static real r__, t;
    static integer ij, il[21], kk, nn, iu[21];
    static real tt, ty, tty;

    /* Fortran I/O blocks */
    static cilist io___2 = { 0, 6, 0, 0, 0 };
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___5 = { 0, 6, 0, 0, 0 };


/* ***purpose  sort an array and optionally make the same interchanges in */
/*            an auxiliary array.  the array may be sorted in increasing */
/*            or decreasing order.  a slightly modified quicksort */
/*            algorithm is used. */

/*   ssort sorts array x and optionally makes the same interchanges in */
/*   array y.  the array x may be sorted in increasing order or */
/*   decreasing order.  a slightly modified quicksort algorithm is used. */

/*   description of parameters */
/*      x - array of values to be sorted */
/*      y - array to be (optionally) carried along */
/*      n - number of values in array x to be sorted */
/*      kflag - control parameter */
/*            =  2  means sort x in increasing order and carry y along. */
/*            =  1  means sort x in increasing order (ignoring y) */
/*            = -1  means sort x in decreasing order (ignoring y) */
/*            = -2  means sort x in decreasing order and carry y along. */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    nn = *n;
    if (nn < 1) {
	s_wsle(&io___2);
	do_lio(&c__9, &c__1, "ssort: The number of sort elements is not posi"
		"tive.", (ftnlen)51);
	e_wsle();
	s_wsle(&io___3);
	do_lio(&c__9, &c__1, "ssort: n = ", (ftnlen)11);
	do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, "   kflag = ", (ftnlen)11);
	do_lio(&c__3, &c__1, (char *)&(*kflag), (ftnlen)sizeof(integer));
	e_wsle();
	return 0;
    }

    kk = abs(*kflag);
    if (kk != 1 && kk != 2) {
	s_wsle(&io___5);
	do_lio(&c__9, &c__1, "the sort control parameter, k, is not 2, 1, -1"
		", or -2.", (ftnlen)54);
	e_wsle();
	return 0;
    }

/*     alter array x to get decreasing order if needed */

    if (*kflag <= -1) {
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    x[i__] = -x[i__];
/* L10: */
	}
    }

    if (kk == 2) {
	goto L100;
    }

/*     sort x only */

    m = 1;
    i__ = 1;
    j = nn;
    r__ = .375f;

L20:
    if (i__ == j) {
	goto L60;
    }
    if (r__ <= .5898437f) {
	r__ += .0390625f;
    } else {
	r__ += -.21875f;
    }

L30:
    k = i__;

/*     select a central element of the array and save it in location t */

    ij = i__ + (integer) ((j - i__) * r__);
    t = x[ij];

/*     if first element of array is greater than t, interchange with t */

    if (x[i__] > t) {
	x[ij] = x[i__];
	x[i__] = t;
	t = x[ij];
    }
    l = j;

/*     if last element of array is less than than t, interchange with t */

    if (x[j] < t) {
	x[ij] = x[j];
	x[j] = t;
	t = x[ij];

/*        if first element of array is greater than t, interchange with t */

	if (x[i__] > t) {
	    x[ij] = x[i__];
	    x[i__] = t;
	    t = x[ij];
	}
    }

/*     find an element in the second half of the array which is smaller */
/*     than t */

L40:
    --l;
    if (x[l] > t) {
	goto L40;
    }

/*     find an element in the first half of the array which is greater */
/*     than t */

L50:
    ++k;
    if (x[k] < t) {
	goto L50;
    }

/*     interchange these elements */

    if (k <= l) {
	tt = x[l];
	x[l] = x[k];
	x[k] = tt;
	goto L40;
    }

/*     save upper and lower subscripts of the array yet to be sorted */

    if (l - i__ > j - k) {
	il[m - 1] = i__;
	iu[m - 1] = l;
	i__ = k;
	++m;
    } else {
	il[m - 1] = k;
	iu[m - 1] = j;
	j = l;
	++m;
    }
    goto L70;

/*     begin again on another portion of the unsorted array */

L60:
    --m;
    if (m == 0) {
	goto L190;
    }
    i__ = il[m - 1];
    j = iu[m - 1];

L70:
    if (j - i__ >= 1) {
	goto L30;
    }
    if (i__ == 1) {
	goto L20;
    }
    --i__;

L80:
    ++i__;
    if (i__ == j) {
	goto L60;
    }
    t = x[i__ + 1];
    if (x[i__] <= t) {
	goto L80;
    }
    k = i__;

L90:
    x[k + 1] = x[k];
    --k;
    if (t < x[k]) {
	goto L90;
    }
    x[k + 1] = t;
    goto L80;

/*     sort x and carry y along */

L100:
    m = 1;
    i__ = 1;
    j = nn;
    r__ = .375f;

L110:
    if (i__ == j) {
	goto L150;
    }
    if (r__ <= .5898437f) {
	r__ += .0390625f;
    } else {
	r__ += -.21875f;
    }

L120:
    k = i__;

/*     select a central element of the array and save it in location t */

    ij = i__ + (integer) ((j - i__) * r__);
    t = x[ij];
    ty = y[ij];

/*     if first element of array is greater than t, interchange with t */

    if (x[i__] > t) {
	x[ij] = x[i__];
	x[i__] = t;
	t = x[ij];
	y[ij] = y[i__];
	y[i__] = ty;
	ty = y[ij];
    }
    l = j;

/*     if last element of array is less than t, interchange with t */

    if (x[j] < t) {
	x[ij] = x[j];
	x[j] = t;
	t = x[ij];
	y[ij] = y[j];
	y[j] = ty;
	ty = y[ij];

/*        if first element of array is greater than t, interchange with t */

	if (x[i__] > t) {
	    x[ij] = x[i__];
	    x[i__] = t;
	    t = x[ij];
	    y[ij] = y[i__];
	    y[i__] = ty;
	    ty = y[ij];
	}
    }

/*     find an element in the second half of the array which is smaller */
/*     than t */

L130:
    --l;
    if (x[l] > t) {
	goto L130;
    }

/*     find an element in the first half of the array which is greater */
/*     than t */

L140:
    ++k;
    if (x[k] < t) {
	goto L140;
    }

/*     interchange these elements */

    if (k <= l) {
	tt = x[l];
	x[l] = x[k];
	x[k] = tt;
	tty = y[l];
	y[l] = y[k];
	y[k] = tty;
	goto L130;
    }

/*     save upper and lower subscripts of the array yet to be sorted */

    if (l - i__ > j - k) {
	il[m - 1] = i__;
	iu[m - 1] = l;
	i__ = k;
	++m;
    } else {
	il[m - 1] = k;
	iu[m - 1] = j;
	j = l;
	++m;
    }
    goto L160;

/*     begin again on another portion of the unsorted array */

L150:
    --m;
    if (m == 0) {
	goto L190;
    }
    i__ = il[m - 1];
    j = iu[m - 1];

L160:
    if (j - i__ >= 1) {
	goto L120;
    }
    if (i__ == 1) {
	goto L110;
    }
    --i__;

L170:
    ++i__;
    if (i__ == j) {
	goto L150;
    }
    t = x[i__ + 1];
    ty = y[i__ + 1];
    if (x[i__] <= t) {
	goto L170;
    }
    k = i__;

L180:
    x[k + 1] = x[k];
    y[k + 1] = y[k];
    --k;
    if (t < x[k]) {
	goto L180;
    }
    x[k + 1] = t;
    y[k + 1] = ty;
    goto L170;

/*     clean up */

L190:
    if (*kflag <= -1) {
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    x[i__] = -x[i__];
/* L200: */
	}
    }
    return 0;
} /* ssort_ */

