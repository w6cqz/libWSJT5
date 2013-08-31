C
C      Copyright (c) 2010...2012 Joe Large W6CQZ
C      based upon wsjt65.f which is;
C      (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.
C
C      License:  V2 GPL
C
      subroutine cqz65(dat,npts,dbw,ddf,afc,wisfile,ical,idf,sym1,sym2,
     +                 sym1p,sym2p,flag,jdf,nsync,nsnr,ddtx)

C     Inputs
C     dat as array(of npts) real
C     npts as integer for size of dat()
C     dbw as integer for decoder bandwidth
C     ddf as integer for decoder center frequency hertz relative to df = 0
C     afc as integer for AFC on = 1 off = 0
C     wisfile as character(255) as path + name of FFTW3 wisdom file
C     ical as integer for control of FFTW wisdom use.  ical = 0 = FFTW_ESTIMATE = 1 = load/use wisdom = 11 = use loaded wisdom, do not reload
C     idf as integer for adjusted DF of decoder
C     Returns
C     sym1(63) as array of integer holding most probable decoded symbols
C     sym2(63) as array of integer holding second most probable decoded symbols
C     sym1p(63) as array of integer holding confidence of symbol being correct for most probable set
C     sym2p(63) as array of integer holding confidence of symbol being correct for second most probable set
C     flag as integer holding flag for bad data = -999 = data is invalid
C     jdf as integer decoded frame DF hertz
C     nsync as integer decoded frame sync quality
C     nsnr as integer decoded frame signal strength in dB
C     ddtx as real (float single) as decoded frame timing variance

C     Orchestrates the process of decoding JT65 messages, using data
C     that have been 2x downsampled.  The search for shorthand messages
C     has already been done.

      real dat(npts)                        !Raw data
      real ccfblue(-5:540),ccfred(-224:224)
      real ddtx
	integer sym1(63),sym2(63),sym1p(63),sym2p(63)
      integer DFTolerance,NAFC,MouseDF,NFreeze,minsync
      integer npts,dbw,ddf,afc,ical,idf,flag,jdf,nsync,nsnr
      character*1 csync
      character*255 wisfile

      logical first
      data first/.true./

      DFTolerance = dbw
      NAFC = afc
      MouseDF = ddf
      flag = 0
      NFreeze=1
      minsync=1 ! nsync measure must be >= minsync
      csync=' '

      if(.not. first) Then
         if(ical.eq.1) ical=11
      endif

      call setup65          !Initialize pseudo-random arrays

      call sync65(dat,npts,DFTolerance,NFreeze,MouseDF,dtx,dfx,snrx,
     +            snrsync,ccfblue,ccfred,flip,width,ical,wisfile)

      nsync=nint(snrsync-3.0)
      nsnr=nint(snrx)

C     The following lines sets the decoder lowest bounds for what might
C     be a signal.  Sync >= 1 and snr >= -33dB = proceed to decode.

      if(nsnr.lt.-33) go to 200
      if(nsync.lt.1) go to 200

      csync='*'
      if(flip.lt.0.0) csync='#'

      call decode65(dat,npts,dtx,dfx,flip,flag,ical,wisfile,sym1,sym2,
     +              sym1p,sym2p)
      
      ndf=nint(dfx)
      jdf=ndf+idf
      ddtx=dtx-1.0

      go to 300

 200  flag=-998

 300  if(first) first=.false.

C     Attempting to fix something that has bothered me for years... snr
C     > -1.

      if(nsnr.gt.-1) nsnr = -1

      return
      end
