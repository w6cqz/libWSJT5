C
C      Copyright (c) 2010 Joe Large W4CQZ
C      based upon wsjt24.f which is;
C      (c) 2001-2010 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.
C
C      License:  V2 GPL
C
      subroutine cqz4(dat,npts,DFTolerance,MouseDF,idf,mline,
     +                ical,wisfile,kvfile)

C  Orchestrates the process of decoding JT2 and JT4 messages, using 
C  data that have been 2x downsampled.  

      real dat(npts)                        !Raw data
      integer DFTolerance
      integer MouseDF
      integer idf
      integer ical
      integer mode
      integer mode4
      integer MinSigDB
      integer NFreeze
      character decoded*22,cooo*3
      character*67 line,mline
      character*1 csync,c1
      character*12 mycall
      character*12 hiscall
      character*6 hisgrid
      character*255 wisfile
      character*255 kvfile
      real ccfblue(-5:540),ccfred(-224:224)

      mode=7 !JT4 (6=JT2)
      mode4=2 !JT4B
      MinSigDB=1
      NFreeze=1
      mycall='            '
      hiscall='            '
      hisgrid='      '

C  Attempt to synchronize: look for sync tone, get DF and DT.

      call sync24(dat,npts,DFTolerance,NFreeze,MouseDF,mode,
     +    mode4,dtx,dfx,snrx,snrsync,flip,width)

      csync=' '
      decoded='                      '
      cooo='   '
      ncount=-1             !Flag for RS decode of current record
      NSyncOK=0
      nqual1=0
      nqual2=0

      nsync=nint(snrsync-3.0)
      nsnr=nint(snrx)
      if(nsnr.lt.-30 .or. nsync.lt.0) nsync=0
      nsnrlim=-32
      if(nsync.lt.MinSigdB .or. nsnr.lt.nsnrlim) go to 200

C  If we get here, we have achieved sync!
      NSyncOK=1
      csync='*'
      if(flip.lt.0.0) csync='#'

      call decode24(dat,npts,dtx,dfx,flip,mode,mode4,decoded,
     +   ncount,qual,ical,wisfile,kvfile)

 200  kvqual=0

      ndf=nint(dfx)
      jdf=ndf+idf

      write(line,2020) jdf,',',nsync,',',nsnr,',',dtx-1.0,',',
     +    csync,',',decoded(1:19)
 2020 format(i5,a1,i3,a1,i3,a1,f4.1,a1,a1,a1,a19)

      mline = ''
      mline = line(1:41)

C      print *,'mline...'
C      print *,mline

 900  continue

      return
      end

