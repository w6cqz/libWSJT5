C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine set(a,y,n)
      real y(n)
      do i=1,n
         y(i)=a
      enddo
      return
      end

      subroutine move(x,y,n)
      real x(n),y(n)
      do i=1,n
         y(i)=x(i)
      enddo
      return
      end

      subroutine zero(x,n)
      real x(n)
      do i=1,n
         x(i)=0.0
      enddo
      return
      end

      subroutine add(a,b,c,n)
      real a(n),b(n),c(n)
      do i=1,n
         c(i)=a(i)+b(i)
      enddo
      return
      end
