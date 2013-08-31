C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine k2grid(k,grid)
      character grid*6

      nlong=2*mod((k-1)/5,90)-179
      if(k.gt.450) nlong=nlong+180
      nlat=mod(k-1,5)+ 85
      dlat=nlat
      dlong=nlong
      call deg2grid(dlong,dlat,grid)

      return
      end
