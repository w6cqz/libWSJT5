C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine wsjt65(dat,npts,cfile6,NClearAve,MinSigdB,
     +  DFTolerance,NFreeze,NAFC,mode65,Nseg,MouseDF,NAgain,
     +  ndepth,neme,idf,idfsh,mycall,hiscall,hisgrid,
     +  lumsg,lcum,nspecial,ndf,nstest,dfsh,snrsh,NSyncOK,
     +  ccfblue,ccfred,ndiag,nwsh,decoded,special,cooo,
     +  mline,csync,nsync,ical,wisfile,kvfile)

C     Orchestrates the process of decoding JT65 messages, using data
C     that have been 2x downsampled.  The search for shorthand messages
C     has already been done.

      real dat(npts)                        !Raw data
      integer DFTolerance
      logical first
      logical lcum
      character decoded*22,cfile6*6,special*5,cooo*3
      character*22 avemsg1,avemsg2,deepmsg
      character*67 line,ave1,ave2
      character*1 csync,c1
      character*12 mycall
      character*12 hiscall
      character*6 hisgrid
      character*67 mline
      character*255 wisfile
      character*255 kvfile
      real ccfblue(-5:540),ccfred(-224:224)
      integer itf(2,9)
      include 'avecom.h'
      data first/.true./,ns10/0/,ns20/0/
      data itf/0,0, 1,0, -1,0, 0,-1, 0,1, 1,-1, 1,1, -1,-1, -1,1/
      save

C      print *,'in wsjt65.f kvfile is;'
C      print *,kvfile

C     Hardcoding NFreeze=1 for now. [DEBUG]
      NFreeze=1
      if(first) then
      call setup65                !Initialize pseudo-random arrays
      nsave=0
      first=.false.
      ave1=' '
      ave2=' '
      endif

      naggressive=0
      if(ndepth.ge.2) naggressive=1
      nq1=3
      nq2=6
      if(naggressive.eq.1) nq1=1

      if(NClearAve.ne.0) then
         nsave=0                     !Clear the averaging accumulators
         ns10=0
         ns20=0
         ave1=' '
         ave2=' '
      endif

      if(MinSigdB.eq.99 .or. MinSigdB.eq.-99) then
         ns10=0                      !For Include/Exclude ?
         ns20=0
      endif

C     Attempt to synchronize: look for sync tone, get DF and DT.
      call sync65(dat,npts,DFTolerance,NFreeze,MouseDF,
     +    mode65,dtx,dfx,snrx,snrsync,ccfblue,ccfred,flip,width,ical,
     +    wisfile)

      csync=' '
      decoded='                      '
      deepmsg='                      '
      special='     '
      cooo='   '
      line=''
      mline=''
      ncount=-1             !Flag for RS decode of current record
      ncount1=-1            !Flag for RS Decode of ave1
      ncount2=-1            !Flag for RS Decode of ave2
      NSyncOK=0
      nqual1=0
      nqual2=0

      if(nsave.lt.MAXAVE .and. (NAgain.eq.0 .or. NClearAve.eq.1))
     +  nsave=nsave+1
      if(nsave.le.0) go to 900          !Prevent bounds error

      nflag(nsave)=0                    !Clear the "good sync" flag
      iseg(nsave)=Nseg                  !Set the RX segment to 1 or 2
      nsync=nint(snrsync-3.0)
      nsnr=nint(snrx)
      if(nsnr.lt.-30 .or. nsync.lt.0) nsync=0
      nsnrlim=-32

C     Good Sync takes precedence over a shorthand message:
      if(nsync.ge.MinSigdB .and. nsnr.ge.nsnrlim .and.
     +   nsync.ge.nstest) nstest=0

      if(nstest.gt.0) then
         dfx=dfsh
         nsync=nstest
         nsnr=snrsh
         dtx=1.
         ccfblue(-5)=-999.0
         if(nspecial.eq.1) special='ATT  '
         if(nspecial.eq.2) special='RO   '
         if(nspecial.eq.3) special='RRR  '
         if(nspecial.eq.4) special='73   '
         NSyncOK=1      !Mark this RX file as good (for "Save Decoded")
         if(NFreeze.eq.0 .or. DFTolerance.ge.200) special(5:5)='?'
         width=nwsh
         idf=idfsh
         go to 200
      endif

      if(nsync.lt.MinSigdB .or. nsnr.lt.nsnrlim) go to 200

C     If we get here, we have achieved sync!
      NSyncOK=1
      nflag(nsave)=1            !Mark this RX file as good
      csync='*'
      if(flip.lt.0.0) then
         csync='#'
         cooo='O ?'
      endif

      call decode65(dat,npts,dtx,dfx,flip,ndepth,neme,
     +   mycall,hiscall,hisgrid,mode65,nafc,decoded,
     +   ncount,deepmsg,qual,ical,wisfile,kvfile)

      if(ncount.eq.-999) qual=0                 !Bad data

 200  kvqual=0
      if(ncount.ge.0) kvqual=1
      nqual=qual
      if(ndiag.eq.0 .and. nqual.gt.10) nqual=10
      if(nqual.ge.nq1 .and.kvqual.eq.0) decoded=deepmsg

      ndf=nint(dfx)
      if(flip.lt.0.0 .and. (kvqual.eq.1 .or. nqual.ge.nq2)) cooo='OOO'
      if(kvqual.eq.0.and.nqual.ge.nq1.and.nqual.lt.nq2) cooo(2:3)=' ?'
      if(decoded.eq.'                      ') cooo='   '
      ! This next bit is confusing in that it seems to be converting
      ! lower case to upper case... how would we even get a lower case
      ! code here?  I'm disabling it to see what happens.
!      do i=1,22
!         c1=decoded(i:i)
!         if(c1.ge.'a' .and. c1.le.'z') decoded(i:i)=char(ichar(c1)-32)
!      enddo
      jdf=ndf+idf
      if(nstest.gt.0) jdf=ndf
      write(line,1010) nsync,',',nsnr,',',dtx-1.0,',',jdf,',',
     +    nint(width),',',csync,',',decoded(1:19)
 1010 format(i3,a1,i3,a1,f4.1,a1,i5,a1,i2,a1,a1,a1,a19)

C     Blank DT if shorthand message  (### wrong logic? ###)
      if(special.ne.'     ') then
          ccfblue(-5)=-9999.0
      else
         nspecial=0
      endif
      mline=line(1:43)
 900  continue
      return
      end
