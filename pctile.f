C     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C     contributions from additional authors.  WSJT is Open Source
C     software, licensed under the GNU General Public License V2 (GPL).
C     Source code and programming information may be found at
C     http://developer.berlios.de/projects/wsjt/.
      subroutine pctile(x,tmp,nmax,npct,xpct)
      real x(nmax),tmp(nmax)

      do i=1,nmax
         tmp(i)=x(i)
      enddo
      call sort(nmax,tmp)
      j=nint(nmax*0.01*npct)
      if(j.lt.1) j=1
      xpct=tmp(j)

      return
      end
