      PROGRAM getpfx1

      character callsign*12
      character*1 c
      character lof*12,rof*12
      integer llof,lrof
      character tpfx*4,tsfx*3
      logical ispfx,issfx,invalid

      tpfx='    '
      tsfx='   '

      ispfx=.false.
      issfx=.false.
      invalid=.false.

      lof='            '
      rof='            '
      llof=0
      rlof=0

      callsign='            '

      Print *,'Input call like ''W4/W6CQZ'' including quotation marks'
      Read *,callsign

      iz=index(callsign,'\')
      if(iz.gt.0) callsign(iz:iz) = '/'

      print *,''
      print *,'Callsign input:  ',callsign
      print *,''

      j=1
      k=1
      
      iz=index(callsign,'/')
      do i=1,12
         if(i.lt.iz) then
            lof(j:j)=callsign(i:i)
            j=j+1
         endif
         if(i.gt.iz) then
            rof(k:k)=callsign(i:i)
            k=k+1
         endif
      enddo

      j = len(lof)
      do while (lof(j:j) .eq. ' ')
         j = j - 1
      enddo
      if(j.lt.0) j=0
      llof=j

      k = len(rof)
      do while (rof(k:k) .eq. ' ')
         k = k - 1
      enddo
      if(k.lt.0) k=0
      lrof=k

C     Now I have the callsign split left of / and right of / into lof and rof
C     If length lof > 4 it can not be a prefix
C     If length rof > 3 it can not be a suffix

      If((llof.gt.0) .and. (llof.le.4)) ispfx=.true.
      If((lrof.gt.0) .and. (lrof.le.3)) issfx=.true.

      If(ispfx .or. issfx) Then
         invalid=.false.
      else
         invalid=.true.
      endif

      If(ispfx .and. issfx) Then
         if(llof.lt.3) issfx=.false.
         if(lrof.lt.3) ispfx=.false.
         if(ispfx .and. issfx) issfx=.false.
      Endif

      If(invalid) Then
         k=-1
      else
         if(ispfx) Then
            tpfx='    '
            do i=1,4
               tpfx(i:i)=lof(i:i)
            enddo
C           Insure upper case
            do i=1,4
               c=tpfx(i:i)
               if(c.ge.'a' .and. c.le.'z')
     +           tpfx(i:i)=char(ichar(c)-ichar('a')+ichar('A'))
            enddo
            k=nchar(tpfx(1:1))
            k=37*k+nchar(tpfx(2:2))
            k=37*k+nchar(tpfx(3:3))
            k=37*k+nchar(tpfx(4:4))
            Print *,'Converting prefix of:  ',tpfx,' to base value ',k
         endif
         if(issfx) Then
            tsfx='   '
            do i=1,3
               tsfx(i:i)=rof(i:i)
            enddo
C           Insure upper case
            do i=1,3
               c=tsfx(i:i)
               if(c.ge.'a' .and. c.le.'z')
     +           tsfx(i:i)=char(ichar(c)-ichar('a')+ichar('A'))
            enddo
            k=nchar(tsfx(1:1))
            print *,k,'   ',nchar(tsfx(1:1))
            k=37*k+nchar(tsfx(2:2))
            print *,k,'   ',nchar(tsfx(2:2))
            k=37*k+nchar(tsfx(3:3))
            print *,k,'   ',nchar(tsfx(3:3))
            Print *,'Converting suffix of:  ',tsfx,' to base value',k
         endif
      endif

      end

