C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine ps(dat,nfft,s,ical,wisfile)

      parameter (NMAX=16384+2)
      parameter (NHMAX=NMAX/2-1)
      character*255 wisfile
      real dat(nfft)
      real s(NHMAX)
      real x(NMAX)
      complex c(0:NHMAX)
      equivalence (x,c)

      nh=nfft/2
      do i=1,nfft
         x(i)=dat(i)/128.0       !### Why 128 ??
      enddo

      call xfft(x,nfft,ical,wisfile)
      fac=1.0/nfft
      do i=1,nh
         s(i)=fac*(real(c(i))**2 + aimag(c(i))**2)
      enddo
      return
      end
