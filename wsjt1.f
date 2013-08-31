C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
        subroutine wsjt1(d,jz0,istart,MyCall,Nseg,MouseDF,DFTolerance,
     +             doshdec,samfacin,Nafc,Nzap,LDecoded,nspecial,ndf,
     +             myline,nsync,ical,wisfile,kvfile)

C     calling params d .. MouseDF are inputs, the rest returns.
C     See four2a.f for explanation of ical value.
C     Routine now REQUIRES floating point input to d()
C     wisfile = path+name of fft wisdom file
C     kvfile = path+name of kv data file

!       lparameter (NP2=1024*1024)
!        parameter (NP2=120*12000)
        parameter (NP2=11025*60)

	real d(jz0)
        integer istart      !Starting location in original d() array
        character FileID*40 !Name of file being processed
        integer MinSigdB    !Minimum ping strength, dB
        integer NQRN        !QRN rejection parameter
        integer NFreeze     !Freeze On/Off 1=On 0=Off
        integer DFTolerance !Defines DF search range
        integer NSyncOK     !Set to 1 if JT65 file synchronized OK
        character*12 mycall
        character*12 hiscall
        character*6 hisgrid
        character decoded*22       ! returns from wsjt65.f
        character special*5,cooo*3 ! ""
        character*43 myline       ! ""
        character*67 mline        ! ""
        character*1 csync          ! ""
        character*255 wisfile
        character*255 kvfile
        real ps0(431)       !Spectrum of best ping
        integer npkept      !Number of pings kept and decoded
        integer lumsg       !Logical unit for decoded.txt
        real basevb         !Baseline signal level, dB
        integer basevbI     !(Integer) Baseline sig level, dB
        integer nslim2      !Minimum strength for single-tone pings, dB
        real psavg(450)     !Average spectrum of the whole file
        integer Nseg        !First or second Tx sequence?
        integer MouseDF     !Freeze position for DF
        logical pick        !True if this is a mouse-picked ping
        logical stbest      !True if the best decode was Single-Tone
        logical STfound     !True if at least one ST decode
        logical LDecoded    !True if anything was decoded
        real s2(64,3100)    !2D spectral array
        real ccf(-5:540)    !X-cor function in JT65 mode (blue line)
        real red(512)
        real ss1(-224:224)  !Magenta curve (for JT65 shorthands)
        real ss2(-224:224)  !Orange curve (for JT65 shorthands)
        real yellow(216)
        real yellow0(216)
        real fzap(200)
        integer resample
        real*8 samfacin,samratio
        real dat2(NP2)
        character msg3*3
        character cfile6*6
        logical lcum
        integer indx(100)
        character*90 line
        common/avecom/dat(NP2),labdat,jza,modea
        common/ccom/nline,tping(100),line(100)
        common/limcom/ nslim2a
        common/extcom/ntdecode
        common/clipcom/ nclip
        save

        MinSigdB=1
        ndepth=0 ! Standard decode, no deep search enabled.
        MouseButton=0
        NClearAve=1
        nforce=0
        NFreeze=1
        mode65=1
        mode4=1
        HisCall=''
        HisGrid=''
        neme=0
        NQRN=5
        Mode=2
        idf=0
        ntdecode0=48
        ntx2=1 ! Disable shorthand message search
        lumsg=11 ! Not used, but defined here for correctness
        nslim2=0
        NAgain=0

        LDecoded=.false.
		basevb=0.
		basevbI=-99
		rmspower=0.
		nspecial=0
		ndf=0
        NSyncOK=0
        npkept=0
        mline = ''
        myline = ''

        lcum=.true.
        jz=jz0
        ntdecode=ntdecode0
        modea=Mode
        nclip=NQRN-5
        nslim2a=nclip
        MinWidth=40                        !Minimum width of pings, ms
        call zero(psavg,450)

        nline=0
        ndiag=0

C     Copy input array d() to dat() so its not modified.
        do j=1,jz
           dat(j)=d(j)
        enddo
        sq=0.
        do j=1,jz                  !Compute power level for whole array
           sq=sq + dat(j)**2
        enddo
        avesq=sq/jz
        basevb=dB(avesq) - 44    !Base power level to send back to GUI
        if(avesq.eq.0) go to 900

        nz=600
        nstep=jz/nz
        sq=0.
        k=0
        do j=1,nz
           sum=0.
           do n=1,nstep
              k=k+1
              sum=sum+dat(k)**2
           enddo
           sum=sum/nstep
           sq=sq + (sum-avesq)**2
        enddo
        rmspower=sqrt(sq/nz)

        pick=.false.
        if(.not.pick .and. nforce.eq.0 .and.
     +     (basevb.lt.-15.0 .or. basevb.gt.20.0)) goto 900
        nchan=64                   !Save 64 spectral channels
        nstep=221                  !Set step size to ~20 ms
        nz=jz/nstep - 1            !# of spectra to compute
        if(.not.pick) then
           MouseButton=0
           jza=jz
           labdat=labdat+1
        endif
        tbest=0.
        NsyncOK=0

C     If we're in JT65 mode, call the decode65 routines. (Which is the
C     only mode that will that exists in this implementation)
        if(mode.eq.2) then
!         if(rmspower.gt.34000.0) go to 900    !Reject very noisy data

C     Check for a JT65 shorthand message
C     This will never execute in this context as I have hard coded ntx2
C     eq 1 since I'm not interested in shorthand messages (for now).
           nstest=0
           if(doshdec.eq.1) ntx2=1
           if(doshdec.eq.0) ntx2=0
C     If ntx2=1 SH decoder will NOT be called.   
C          ntx2=1
           if(ntx2.ne.1) call short65(dat,jz,NFreeze,MouseDF,
     +        DFTolerance,mode65,nspecial,nstest,dfsh,iderrsh,
     +        idriftsh,snrsh,ss1,ss2,nwsh,idfsh,ical,wisfile)
C     Lowpass filter and decimate by 2
           call lpf1(dat,jz,jz2,MouseDF,MouseDF2,ical,wisfile)
           idf=mousedf-mousedf2
           jz=jz2
           nadd=1
           fzap(1)=0.
          if(nzap.eq.1) call avesp2(dat,jz,nadd,mode,NFreeze,MouseDF2,
     +       DFTolerance,fzap,ical,wisfile)

          if(nzap.eq.1.and.nstest.eq.0) call bzap(dat,jz,nadd,mode,fzap,
     +       ical,wisfile)

           i=index(MyCall,char(0))
           if(i.le.0) i=index(MyCall,' ')
           mycall=MyCall(1:i-1)//'            '
           i=index(HisCall,char(0))
           if(i.le.0) i=index(HisCall,' ')
           hiscall=HisCall(1:i-1)//'            '

!     Offset data by about 1 s.
!          jztest=126*2048
           jztest=11025*ntdecode/2 - 2755
           if(jz.ge.jztest) call wsjt65(dat(4097),jz-4096,cfile6,
     +        NClearAve,MinSigdB,DFTolerance,NFreeze,NAFC,mode65,Nseg,
     +        MouseDF2,NAgain,ndepth,neme,idf,idfsh,mycall,hiscall,
     +        hisgrid,lumsg,lcum,nspecial,ndf,nstest,dfsh,snrsh,
     +        NSyncOK,ccf,psavg,ndiag,nwsh,decoded,special,cooo,
     +        mline,csync,nsync,ical,wisfile,kvfile)
           goto 900
        endif

 900    LDecoded=((NSyncOK.gt.0) .or. npkept.gt.0)
        myline='                                           '
        myline=mline(1:43)
        return
        end

