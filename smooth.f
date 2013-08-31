C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine smooth(x,nz)

      real x(nz)
      integer nz

      x0=x(1)
      do i=2,nz-1
         x1=x(i)
         x(i)=0.5*x(i) + 0.25*(x0+x(i+1))
         x0=x1
      enddo

      return
      end
