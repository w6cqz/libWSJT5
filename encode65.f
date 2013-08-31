C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine encode65(message,sent)

      character message*22
      integer dgen(12)
      integer sent(63)

      call packmsg(message,dgen)
      call rs_encode(dgen,sent)
      call interleave63(sent,1)
      call graycode(sent,63,1)

      return
      end
