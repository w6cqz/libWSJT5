C     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C     contributions from additional authors.  WSJT is Open Source
C     software, licensed under the GNU General Public License V2 (GPL).
C     Source code and programming information may be found at
C     http://developer.berlios.de/projects/wsjt/.
      subroutine graycode(dat,n,idir)

      integer dat(n)
      do i=1,n
         dat(i)=igray(dat(i),idir)
      enddo

      return
      end

