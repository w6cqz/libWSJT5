C     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C     contributions from additional authors.  WSJT is Open Source
C     software, licensed under the GNU General Public License V2 (GPL).
C     Source code and programming information may be found at
C     http://developer.berlios.de/projects/wsjt/.
      subroutine interleave63(d1,idir)

C     Interleave (idir=1) or de-interleave (idir=-1) the array d1.

      integer d1(0:6,0:8)
      integer d2(0:8,0:6)

      if(idir.ge.0) then
         do i=0,6
            do j=0,8
               d2(j,i)=d1(i,j)
            enddo
         enddo
         call move(d2,d1,63)
      else
         call move(d1,d2,63)
         do i=0,6
            do j=0,8
               d1(i,j)=d2(j,i)
            enddo
         enddo
      endif

      return
      end
