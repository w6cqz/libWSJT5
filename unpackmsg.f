C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine unpackmsg(dat,msg)

      parameter (NBASE=37*36*10*27*27*27)
      parameter (NGBASE=180*180)
      integer dat(12)
      character c1*12,c2*12,grid*4,msg*22,grid6*6
      logical cqnnn

      cqnnn=.false.
C     I'm going to go ahead and initialize all the return variables to
C     empty results.  Probably overkill, but it may save a lot of debug
C     time if I start getting odd returns as I modify this code.
      c1='            '
      c2='            '
      grid='    '
      grid6='      '
      msg='                      '
      nc1=0
      nc2=0
      ng=0

C     Take the 12 6 bit integer symbols in dat(1..12) and convert to
C     nc1, nc2 and ng which, if I've got this right, are 3 24 bit
C     integers representing the 72 bit packed message. if ng <= 32768
C     then nc1 holds the TX station call + either CQ or QRZ prepended,
C     nc2 holds the RX station call and ng holds the transmitting
C     stations grid or signal report or prefix/suffix data.  With ng
C     the decoded data will be any one of the 3 choices, but only one.
C     In the case of ng > 32768 nc1, nc2 and ng will, together, yield
C     the plain text message with a limit of X characters of the JT65
C     allowable character set.

      nc1=ishft(dat(1),22) + ishft(dat(2),16) + ishft(dat(3),10)+
     +  ishft(dat(4),4) + iand(ishft(dat(5),-2),15)

      nc2=ishft(iand(dat(5),3),26) + ishft(dat(6),20) +
     +  ishft(dat(7),14) + ishft(dat(8),8) + ishft(dat(9),2) +
     +  iand(ishft(dat(10),-4),3)

      ng=ishft(iand(dat(10),15),12) + ishft(dat(11),6) + dat(12)

C     if ng > 32768 a plain text message has been received
      if(ng.gt.32768) then
         call unpacktext(nc1,nc2,ng,msg)
         go to 100
      endif

      if(nc1.lt.NBASE) then
         ! nc1 contains only a callsign
         call unpackcall(nc1,c1)
      else
         ! nc1 contains a callsign + a prepend (CQ or QRZ)
         c1='......'
         if(nc1.eq.NBASE+1) c1='CQ    '
         if(nc1.eq.NBASE+2) c1='QRZ   '
         ! check to see if it is a cq with a callback qrg offset
         nfreq=nc1-NBASE-3
         if(nfreq.ge.0 .and. nfreq.le.999) then
            !write(c1,1002) nfreq
 1002       format('CQ ',i3.3)
            cqnnn=.true.
         endif
      endif

      if(nc2.lt.NBASE) then
         ! nc2 contains a callsign
         call unpackcall(nc2,c2)
      else
         ! nc2 does not contain a callsign
         c2='......'
      endif

C     That leaves ng to deal with... ng can contain a grid encoded as a
C     lat/long pair or a prefix and/or suffix to a callsign or a signal
C     report or a standard response of RRR or RO or 73.  ng will yield
C     any one of the choices above, but only one.

      call unpackgrid(ng,grid)
      grid6=grid//'ma'
      call grid2k(grid6,k)
      if(k.ge.1 .and. k.le.450)   call getpfx2(k,c1)
      if(k.ge.451 .and. k.le.900) call getpfx2(k,c2)

C     At this point c1 has the decode of nc1, c2 of nc2 and grid6 of ng

C     INDEX(STRING, SUBSTRING, BACK)
C     Returns the starting position of a substring within a string.
C
C     Argument type and attributes
C
C     STRING
C       must be of type character.
C     SUBSTRING
C       must be of type character with the same kind type parameter
C       as STRING.
C     BACK (optional)
C       must be of type logical.
C
C     Result type and attributes
C
C     Default integer.
C     Result value
C
C     Case (i): If BACK is absent or present with the value .FALSE.,
C               the result is the minimum positive value of I such that
C               STRING (I : I + LEN (SUBSTRING) - 1) = SUBSTRING or
C               zero if there is no such value. Zero is returned if LEN
C               (STRING) < LEN (SUBSTRING). One is returned if LEN
C               (SUBSTRING) = 0.
C
C     Case (ii): If BACK is present with the value .TRUE., the result
C                is the maximum value of I less than or equal to
C                LEN (STRING) - LEN (SUBSTRING) + 1, such that
C                STRING (I : I + LEN (SUBSTRING) - 1) =  SUBSTRING or
C                zero if there is no such value. Zero is returned if
C                LEN (STRING) < LEN (SUBSTRING) and LEN (STRING) + 1 is
C                returned if LEN (SUBSTRING) = 0.
C
C     Examples
C
C     INDEX ('FORTRAN', 'R') has the value 3.
C
C     INDEX ('FORTRAN', 'R', BACK = .TRUE.) has the value 5.

C     It seems to be looking for the first occurence of a null (0)
C     character value.
      i=index(c1,char(0))
C     Now if a null was found at >= position 3 then add the space block
      if(i.ge.3) c1=c1(1:i-1)//'            '
      i=index(c2,char(0))
      if(i.ge.3) c2=c2(1:i-1)//'            '
C     Clearing the msg in prep for constructing it from c1,c2,grid6

C     I'm really starting to think I'd like to return c1,c2 and grid6
C     as distinct return values and do away with all this formatting
C     mess.  It makes sense to do the formatting within WSJT but for
C     the library I think I'd rather simply have the distinct chunks.
C     Of course, that means a lot of changes all the way back to
C     wsjt1.f <sigh>

      msg='                      ' ! yup, 22 spaces which is length(msg)
      j=0
      if(cqnnn) then
         ! This condition is met with a CQ + a QRG offset in c1
         msg=c1//'                ' ! c1 + 16 spaces
         j=7                                  !### ??? ###
         go to 10
      endif

      do i=1,12
         j=j+1
         msg(j:j)=c1(i:i)
         if(c1(i:i).eq.' ') go to 10
      enddo
      j=j+1
      msg(j:j)=' '

 10   do i=1,12
         if(j.le.21) j=j+1
         msg(j:j)=c2(i:i)
         if(c2(i:i).eq.' ') go to 20
      enddo
      j=j+1
      msg(j:j)=' '

 20   if(k.eq.0) then
         do i=1,4
            if(j.le.21) j=j+1
            msg(j:j)=grid(i:i)
         enddo
         j=j+1
         msg(j:j)=' '
      endif

 100  return
      end
