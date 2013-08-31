C      (c) 2010...2012 J C Large - W6CQZ - GPL 2
C
C      modified code from WSJT which is;
C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
      subroutine msync65(dat,jz,syncount,dtxa,dfxa,snrxa,snrsynca,
     +                   ical,wisfile)

C Synchronizes JT65 data, finding the best-fit DT and DF.
C NB: at this stage, submodes ABC are processed in the same way.

      parameter (NP2=60*11025)    !Size of data array
      parameter (NFFTMAX=2048)    !Max length of FFTs
      parameter (NHMAX=NFFTMAX/2) !Max length of power spectra
      parameter (NSMAX=320)       !Max number of half-symbol steps
      real dat(jz)
      real psavg(NHMAX)           !Average spectrum of whole record
      real s2(NHMAX,NSMAX)        !2d spectrum, stepped by half-symbols
      real ccfblue(-5:540)        !CCF with pseudorandom sequence
      real snrsynca(255)
      real snrxa(255)
      real dfxa(255)
      real dtxa(255)
      integer jz,syncount,ical,DFTolerance            
      character*255 wisfile

      ! The value 450 is empirical:
      real ccfred(-450:450)       !Peak of ccfblue, as function of freq
      real ccfred1(-224:224)      !Peak of ccfblue, as function of freq
      real tmp(450)
      integer nsync
      integer nsnr
      integer lagpkx
      save

      ! Clear return arrays
      do j=1,255
         dfxa(j)=0.0
         dtxa(j)=0.0
         snrsynca(j)=0.0
         snrxa(j)=0.0
      enddo
      mode65=1
      ! Do FFTs of symbol length, stepped by half symbols.  Note that we
      ! have already downsampled the data by factor of 2.
      nsym=126
      nfft=2048
      nsteps=2*jz/nfft - 1
      nh=nfft/2
      df=0.5*11025.0/nfft

      ! Compute power spectrum for each step and get average
      call zero(psavg,nh)
      do j=1,nsteps
         k=(j-1)*nh + 1
         call limit(dat(k),nfft)
         call ps(dat(k),nfft,s2(1,j),ical,wisfile)
         if(mode65.eq.4) call smooth(s2(1,j),nh)
         call add(psavg,s2(1,j),psavg,nh)
      enddo

      call flat1(psavg,s2,nh,nsteps,NHMAX,NSMAX)        !Flatten the spectra

      ! Find the best frequency channel for CCF
      famin=3.
      fbmax=2700.
      fa=famin
      fb=fbmax

      ! Hard coding this to a 1K BW centered on 1270.46
      NFreeze=1
      MouseDF=0
      DFTolerance=1000

      if(NFreeze.eq.1) then
         fa=max(famin,1270.46+MouseDF-DFTolerance)
         fb=min(fbmax,1270.46+MouseDF+DFTolerance)
      else
         fa=max(famin,1270.46+MouseDF-600)
         fb=min(fbmax,1270.46+MouseDF+600)
      endif

      ia=fa/df
      ib=fb/df
      i0=nint(1270.46/df)
      lag1=-5
      lag2=59
      syncbest=-1.e30
      syncbest2=-1.e30
      syncount=0
      call zero(ccfred,745)

      do i=ia,ib
         call xcor(s2,i,nsteps,nsym,lag1,lag2,
     +        ccfblue,ccf0,lagpkx,flip,0.0)
         j=i-i0
         if(j.ge.-372 .and. j.le.372) ccfred(j)=ccf0
         ! Find rms of the CCF, without the main peak
         call slope(ccfblue(lag1),lag2-lag1+1,lagpkx-lag1+1.0)
         !sync=abs(ccfblue(lagpkx))
         ppmax=psavg(i)-1.0

         if(ppmax.gt.0.2938) then

		lagpk=lagpkx
            ipk = i
            ! Peak up in frequency to fraction of channel
            base=0.25*(psavg(ipk-3)+psavg(ipk-2)+psavg(ipk+2)+
     +                 psavg(ipk+3))
            dx=0.
            dfx=(ipk+dx-i0)*df

            ! Peak up in time, at best whole-channel frequency
            call xcor(s2,ipk,nsteps,nsym,lag1,lag2,
     +                ccfblue,ccfmax,lagpk,flip,0.0)
            xlag=lagpk
            if(lagpk.gt.lag1 .and. lagpk.lt.lag2) then
               call peakup(ccfblue(lagpk-1),ccfmax,ccfblue(lagpk+1),dx2)
               xlag=lagpk+dx2
            endif

            ! Find rms of the CCF, without the main peak
            call slope(ccfblue(lag1),lag2-lag1+1,xlag-lag1+1.0)
            sq=0.
            nsq=0
            do lag=lag1,lag2
               if(abs(lag-xlag).gt.2.0) then
                  sq=sq+ccfblue(lag)**2
                  nsq=nsq+1
               endif
            enddo
            rms=sqrt(sq/nsq)
            snrsync=abs(ccfblue(lagpk))/rms - 1.1    !Empirical
            dt=2.0/11025.0
            istart=xlag*nh
            dtx=istart*dt
            snrx=-99.0
            ppmax=psavg(ipk)-1.0
            ! Plus 3 dB because sync tone is on half the time.  (Don't
            ! understand why an additional +2 dB is needed ...)
            if(ppmax.gt.0.0001) snrx=db(ppmax*df/2500.0) + 5.0    !###
            if(mode65.eq.4) snrx=snrx + 2.0
            if(snrx.lt.-33.0) snrx=-33.0
            nsync=nint(snrsync-3.0)
            nsnr=nint(snrx)
            if(nsnr.lt.-30 .or. nsync.lt.0) nsync=0
            if(nsync.gt.0) Then
               if(syncount.lt.256) Then
                  syncount=syncount+1
                  snrsynca(syncount)=snrsync
                  snrxa(syncount)=snrx
                  dtxa(syncount)=dtx
                  dfxa(syncount)=dfx
               endif
            endif
         endif
      enddo

C      print *,'Leaving msync65'
C      print *,''

      return
      end

