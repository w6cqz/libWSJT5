C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine flat1(psavg,s2,nh,nsteps,nhmax,nsmax)

      real psavg(nh)
      real s2(nhmax,nsmax)
      real x(8192),tmp(33)

      nsmo=33
      ia=nsmo/2 + 1
      ib=nh - nsmo/2 - 1
      do i=ia,ib
         call pctile(psavg(i-nsmo/2),tmp,nsmo,50,x(i))
      enddo
      do i=1,ia-1
         x(i)=x(ia)
      enddo
      do i=ib+1,nh
         x(i)=x(ib)
      enddo

      do i=1,nh
         psavg(i)=psavg(i)/x(i)
         do j=1,nsteps
            s2(i,j)=s2(i,j)/x(i)
         enddo
      enddo

      return
      end


