C     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C     contributions from additional authors.  WSJT is Open Source
C     software, licensed under the GNU General Public License V2 (GPL).
C     Source code and programming information may be found at
C     http://developer.berlios.de/projects/wsjt/.
      subroutine chkhist(mrsym,nmax,ipk)

      integer mrsym(63)
      integer hist(0:63)
      integer ipk
      integer nmax

      do i=0,63
         hist(i)=0
      enddo

      do j=1,63
         i=mrsym(j)
         hist(i)=hist(i)+1
      enddo

      nmax=0

      do i=0,63
         if(hist(i).gt.nmax) then
            nmax=hist(i)
            ipk=i+1
         endif
      enddo

      return
      end
