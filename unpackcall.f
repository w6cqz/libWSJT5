C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine unpackcall(ncall,word)

      character word*12,c*37

      data c/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ '/

      n=ncall
      word='......'
      if(n.ge.262177560) go to 999            !Plain text message ...
      i=mod(n,27)+11
      word(6:6)=c(i:i)
      n=n/27
      i=mod(n,27)+11
      word(5:5)=c(i:i)
      n=n/27
      i=mod(n,27)+11
      word(4:4)=c(i:i)
      n=n/27
      i=mod(n,10)+1
      word(3:3)=c(i:i)
      n=n/10
      i=mod(n,36)+1
      word(2:2)=c(i:i)
      n=n/36
      i=n+1
      word(1:1)=c(i:i)
      do i=1,4
         if(word(i:i).ne.' ') go to 10
      enddo
      go to 999
 10   word=word(i:)

 999  if(word(1:3).eq.'3D0') word='3DA0'//word(4:)
      return
      end
