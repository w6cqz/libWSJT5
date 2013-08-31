C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      function nchar(c)

C  Convert ascii number, letter, or space to 0-36 for callsign packing.

      character c*1

      if(c.ge.'0' .and. c.le.'9') then
         n=ichar(c)-ichar('0')
      else if(c.ge.'A' .and. c.le.'Z') then
         n=ichar(c)-ichar('A') + 10
      else if(c.ge.'a' .and. c.le.'z') then
         n=ichar(c)-ichar('a') + 10
      else if(c.ge.' ') then
         n=36
      else
         Print*,'Invalid character in callsign ',c,' ',ichar(c)
         stop
      endif
      nchar=n

      return
      end
