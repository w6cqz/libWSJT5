C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine grid2k(grid,k)

      character*6 grid

      call grid2deg(grid,xlong,xlat)
      nlong=nint(xlong)
      nlat=nint(xlat)
      k=0
      if(nlat.ge.85) k=5*(nlong+179)/2 + nlat-84

      return
      end
