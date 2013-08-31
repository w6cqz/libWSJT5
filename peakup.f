C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine peakup(ym,y0,yp,dx)

      b=(yp-ym)/2.0
      c=(yp+ym-2.0*y0)/2.0
      dx=-b/(2.0*c)

      return
      end
