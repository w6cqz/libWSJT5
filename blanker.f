C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine blanker(d2d,jz)

      integer*2 d2d(jz)

      avg=700.
      threshold=5.0
      do i=1,jz
         xmag=abs(d2d(i))
         xmed=0.75*xmed + 0.25*d2d(i)
         avg=0.999*avg + 0.001*xmag
         if(xmag.gt.threshold*avg) then
!            d2d(i)=nint(xmed)
            d2d(i)=0
         endif
      enddo

      return
      end
