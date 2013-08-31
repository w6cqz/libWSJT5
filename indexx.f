C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine indexx(n,arr,indx)

      parameter (NMAX=3000)
      integer indx(n)
      real arr(n)
      real brr(NMAX)
      if(n.gt.NMAX) then
         print*,'n=',n,' too big in indexx.'
         stop
      endif
      do i=1,n
         brr(i)=arr(i)
         indx(i)=i
      enddo
      call ssort(brr,indx,n,2)

      return
      end

