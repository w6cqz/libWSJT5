      PROGRAM getpfx1

      character word*12,c*37,form*4,psfx*4

      data c/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ '/

      ncall = 267693193

      Print *,ncall

      form='    '
      psfx='    '

      n=ncall
      word='......'
      if(n.ge.262177560) go to 888
      form='CALL'
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

      go to 999

 888  if(n.ge.267796946) goto 1000
      

C     Have an extended n here.
C     CQ  with Prefix 262,178,563 ... 264,002,071
C     QRZ with Prefix 264,002,072 ... 265,825,580
C     DE  with Prefix 265,825,581 ... 267,649,089
C    
C     CQ  with Suffix 267,649,090 ... 267,698,374
C     QRZ with Suffix 267,698,375 ... 267,747,659
C     DE  with Suffix 267,747,660 ... 267,796,944

C     NOTE NOTE NOTE NOTE The prefix/suffix strings decode
C     in REVERSE order (right to left character ordering).


      if((n.ge.262178563) .and. (n.le.264002071)) Then
C        CQ with prefix
         form ='CQ  '
         Print *,'CQ with prefix'
         n=n-262178563
         i=mod(n,37)+1
         psfx(4:4)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(3:3)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(2:2)=c(i:i)
         n=n/37
         i=n+1
         psfx(1:1)=c(i:i)
         Print *,form,psfx
      endif

      if((n.ge.264002072) .and. (n.le.265825580)) Then
C        QRZ with prefix
         form ='QRZ '
         Print *,'QRZ with prefix'
         n=n-264002072
         i=mod(n,37)+1
         psfx(4:4)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(3:3)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(2:2)=c(i:i)
         n=n/37
         i=n+1
         psfx(1:1)=c(i:i)
         Print *,form,psfx
      endif

      if((n.ge.265825581) .and. (n.le.267649089)) Then
C        DE with prefix
         form ='DE  '
         Print *,'DE with prefix'
         n=n-265825581
         i=mod(n,37)+1
         psfx(4:4)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(3:3)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(2:2)=c(i:i)
         n=n/37
         i=n+1
         psfx(1:1)=c(i:i)
         Print *,form,psfx
      endif

      if((n.ge.267649090) .and. (n.le.267698374)) Then
C        CQ with suffix
         form ='CQ  '
         Print *,'CQ with suffix'
         n=n-267649090
         i=mod(n,37)+1
         psfx(3:3)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(2:2)=c(i:i)
         n=n/37
         i=n+1
         psfx(1:1)=c(i:i)
         Print *,form,psfx
      endif

      if((n.ge.267698375) .and. (n.le.267747659)) Then
C        QRZ with suffix
         form ='QRZ '
         Print *,'QRZ with suffix'
         n=n-267698375
         i=mod(n,37)+1
         psfx(3:3)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(2:2)=c(i:i)
         n=n/37
         i=n+1
         psfx(1:1)=c(i:i)
         Print *,form,psfx
      endif

      if((n.ge.267747660) .and. (n.le.267796944)) Then
C        DE with suffix
         form ='DE  '
         Print *,'DE with suffix'
         n=n-267747660
         i=mod(n,37)+1
         psfx(3:3)=c(i:i)
         n=n/37
         i=mod(n,37)+1
         psfx(2:2)=c(i:i)
         n=n/37
         i=n+1
         psfx(1:1)=c(i:i)
         Print *,form,psfx
      endif

      if(n.eq.267796945) Then
C        DE with no prefix or suffix
         Print *,'DE with nothing'
         form = 'DE  '
         psfx = '    '
         Print *,form,psfx
      endif

 999  if(word(1:3).eq.'3D0') word='3DA0'//word(4:)
      go to 1001

C      return

 1000 Print *,'Overflow in n'

 1001 end
