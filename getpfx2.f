C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine getpfx2(k0,callsign)

      character callsign*12
      include 'pfx.f'
      character addpfx*8
      common/gcom4/addpfx

      k=k0
      if(k.gt.450) k=k-450
      if(k.ge.1 .and. k.le.NZ) then
         iz=index(pfx(k),' ') - 1
         callsign=pfx(k)(1:iz)//'/'//callsign
      else if(k.ge.401 .and. k.le.400+NZ2) then
         iz=index(callsign,' ') - 1
         callsign=callsign(1:iz)//'/'//sfx(k-400)
      else if(k.eq.449) then
         iz=index(addpfx,' ') - 1
         if(iz.lt.1) iz=8
         callsign=addpfx(1:iz)//'/'//callsign
      endif

      return
      end

