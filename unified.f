C
C      Copyright (c) 2010...2012 Joe Large W6CQZ
C      based upon wsjt65.f which is;
C      (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.
C
C      License:  V2 GPL
C
C      subroutine cqz65(dat,da,npts,DFTolerance,NAFC,MouseDF,idf,mline,
C     +                 kvfile,wisfile,ical)
      subroutine ucqz65(dat,npts,dbw,ddf,afc,kvfile,wisfile,ical,idf,
     +                  sym1,sym2,sym1p,sym2p,ncount)

C     Inputs
C     dat as array(of npts) real
C     npts as integer for size of dat()
C     dbw as integer for decoder bandwidth
C     ddf as integer for decoder center frequency hertz relative to df = 0
C     afc as integer for AFC on = 1 off = 0
C     kvfile as character(255) as path + name of kvasd data file
C     wisfile as character(255) as path + name of FFTW3 wisdom file
C     ical as integer for control of FFTW wisdom use.  ical = 0 = FFTW_ESTIMATE = 1 = load/use wisdom = 11 = use loaded wisdom, do not reload
C     Returns
C     idf as integer for adjusted DF of decoder
C     sym1(63) as array of integer holding most probable decoded symbols
C     sym2(63) as array of integer holding second most probable decoded symbols
C     sym1p(63) as array of integer holding confidence of symbol being correct for most probable set
C     sym2p(63) as array of integer holding confidence of symbol being correct for second most probable set
C     ncount as integer holding flag for bad dat = -999 = data is invalid

C     Orchestrates the process of decoding JT65 messages, using data
C     that have been 2x downsampled.  The search for shorthand messages
C     has already been done.

      real dat(npts)                        !Raw data
      integer DFTolerance
      integer da(12)
      character decoded*22,special*5
      character*22 avemsg1,avemsg2,deepmsg
      character*67 line,ave1,ave2,line2
      character*1 csync,c1
      character*12 mycall
      character*12 hiscall
      character*6 hisgrid
      character*67 mline
      character*255 wisfile
      character*255 kvfile
      real ccfblue(-5:540),ccfred(-224:224)
      integer nprc(126)
      integer nfreeze
      logical haveconfig
      logical first
      common/prcom/pr(135),mdat(126),mref(126,2),mdat2(126),mref2
      data haveconfig/.false./,first/.true./,mr2/0/

      data nprc/
     + 1,0,0,1,1,0,0,0,1,1,1,1,1,1,0,1,0,1,0,0,
     + 0,1,0,1,1,0,0,1,0,0,0,1,1,1,0,0,1,1,1,1,
     + 0,1,1,0,1,1,1,1,0,0,0,1,1,0,1,0,1,0,1,1,
     + 0,0,1,1,0,1,0,1,0,1,0,0,1,0,0,0,0,0,0,1,
     + 1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,1,0,1,
     + 0,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,0,1,1,
     + 1,1,1,1,1,1/

      common/prcom/pr(135),mdat(126),mref(126,2),mdat2(126),mref2(126,2)

      DFTolerance = dbw
      NAFC = afc
      MouseDF = ddf

      print *,'kvfile:'
      print *,kvfile
      print *,''
      print *,'wisfile:'
      print *,wisfile
      print *,''
      print *,'ical:'
      print *,ical
      print *,''
      if(first) print *,'First call'
      if(.not. first) print *,'Not first call'

      haveconfig=.true.

      if(.not. first) Then
         if(ical.eq.1) ical=11
      endif

      NFreeze=1
      NClearAve=1
      mode65=1
      MinSigDB=1
      ndepth=0 ! Standard decode, no deep search enabled.
      neme=0
      naggressive=0
      nstest=0
      ndiag=0
      mycall='            '
      hiscall='            '
      hisgrid='      '
      csync=' '
      decoded='                      '
      deepmsg='                      '
      special='     '
      line=''
      line2=''
      mline=''
      ncount=-1             !Flag for RS decode of current record
      NSyncOK=0
      nqual1=0
      nqual2=0

C      call setup65          !Initialize pseudo-random arrays
C     Put the appropriate pseudo-random sequence into pr
      nsym=126
      do i=1,nsym
         pr(i)=2*nprc(i)-1
      enddo

C     Determine locations of data and reference symbols
      k=0
      mr1=0
      do i=1,nsym
         if(pr(i).lt.0.0) then
            k=k+1
            mdat(k)=i
         else
            mr2=i
            if(mr1.eq.0) mr1=i
         endif
      enddo
      nsig=k

C     Determine the reference symbols for each data symbol.
      do k=1,nsig
         m=mdat(k)
         mref(k,1)=mr1
         do n=1,10                     !Get ref symbol before data
            if((m-n).gt.0) then
               if (pr(m-n).gt.0.0) go to 1
            endif
         enddo
         go to 2
 1       mref(k,1)=m-n
 2       mref(k,2)=mr2
         do n=1,10                     !Get ref symbol after data
            if((m+n).le.nsym) then
               if (pr(m+n).gt.0.0) go to 3
            endif
         enddo
         go to 4
 3       mref(k,2)=m+n
 4       continue
      enddo

C     Now do it all again, using opposite logic on pr(i)
      k=0
      mr1=0
      do i=1,nsym
         if(pr(i).gt.0.0) then
            k=k+1
            mdat2(k)=i
         else
            mr2=i
            if(mr1.eq.0) mr1=i
         endif
      enddo
      nsig=k

      do k=1,nsig
         m=mdat2(k)
         mref2(k,1)=mr1
         do n=1,10
            if((m-n).gt.0) then
               if (pr(m-n).lt.0.0) go to 5
            endif
         enddo
         go to 6
 5       mref2(k,1)=m-n
 6       mref2(k,2)=mr2
         do n=1,10
            if((m+n).le.nsym) then
               if (pr(m+n).lt.0.0) go to 7
            endif
         enddo
         go to 8
 7       mref2(k,2)=m+n
 8       continue
      enddo

      call sync65(dat,npts,DFTolerance,NFreeze,MouseDF,
     +    mode65,dtx,dfx,snrx,snrsync,ccfblue,ccfred,flip,width,ical,
     +    wisfile)

      nsync=nint(snrsync-3.0)
      nsnr=nint(snrx)
      if(nsnr.lt.-30 .or. nsync.lt.0) nsync=0
      nsnrlim=-32

      if(nsync.lt.MinSigdB .or. nsnr.lt.nsnrlim) go to 200

      csync='*'
      if(flip.lt.0.0) csync='#'

      call decode65(dat,da,npts,dtx,dfx,flip,ndepth,neme,
     +   mycall,hiscall,hisgrid,mode65,nafc,decoded,
     +   ncount,deepmsg,qual,ical,wisfile,kvfile)

      if(ncount.eq.-999) qual=0                 !Bad data

 200  kvqual=0

      ndf=nint(dfx)
      jdf=ndf+idf

      write(line,2020) jdf,',',nsync,',',nsnr,',',dtx-1.0,',',
     +    csync,',',decoded(1:21)

 2020 format(i5,a1,i3,a1,i3,a1,f4.1,a1,a1,a1,a21)

      mline=line(1:43)

 900  continue

      if(first) first=.false.

      return
      end
