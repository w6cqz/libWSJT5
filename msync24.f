C      (c) 2010 J C Large - W4CQZ - GPL 2
C      modified code from WSJT which is;
C      Copyright (c) 2001-2010 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C
      subroutine msync4(dat,jz,syncount,dtxa,dfxa,snrxa,snrsynca,
     +  ical,wisfile)

      !Synchronizes JT2 and JT4 data, finding the best-fit DT and DF.  
      parameter (NFFTMAX=2520)         !Max length of FFTs
      parameter (NHMAX=NFFTMAX/2)      !Max length of power spectra
      parameter (NSMAX=525)            !Max number of half-symbol steps
      integer DFTolerance              !Range of DF search
      real dat(jz)
      real psavg(NHMAX)                !Average spectrum of whole record
      real s2(NHMAX,NSMAX)             !2d spectrum, stepped by half-symbols
      real ccfblue(-5:540)             !CCF with pseudorandom sequence
      real ccfred(-450:450)            !Peak of ccfblue, as function of freq
      real ccfred1(-224:224)           !Peak of ccfblue, as function of freq
      real tmp(550)
      integer syncount                 !Number of sync points found
      integer ical
      character*255 wisfile
      real snrsynca(255)
      real snrxa(255)
      real dfxa(255)
      real dtxa(255)
      integer nsync
      integer nsnr
      integer lagpkx
      save ! Necessary?  Check to conf.

      ! Clear return arrays
      do j=1,255
         dfxa(j)=0.0
         dtxa(j)=0.0
         snrsynca(j)=0.0
         snrxa(j)=0.0
      enddo
      syncount=0

      !Do FFTs of twice symbol length, stepped by half symbols.  Note that 
      !we have already downsampled the data by factor of 2.

      mode=7 !JT4
      mode4=2 !JT4B
      nsym=207
      nfft=2520
      nh=nfft/2
      nq=nfft/4
      nsteps=jz/nq - 1
      df=0.5*11025.0/nfft
      call zero(psavg,nh)

      !Compute power spectrum for each step and get average
      do j=1,nsteps
         k=(j-1)*nq + 1
         call ps24(dat(k),nfft,s2(1,j),ical,wisfile)
         call add(psavg,s2(1,j),psavg,nh)
      enddo

      call flat1(psavg,s2,nh,nsteps,NHMAX,NSMAX)        !Flatten the spectra

      !Find the best frequency channel for CCF
      famin=200.
      fbmax=2700.

      fa=famin
      fb=fbmax

      !Hard coding this to a +/-350Hz BW centered on 1270.46
      !This may be still too much for JT4 use.... but, assuming
      !JT4A BW ~ 17.5Hz and leaving room for 17.5Hz between each
      !JT4A signal yiels 35Hz spacing yielding room for 20 non-
      !overlapped JT4A signals in a 700Hz passband.
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
      if(mode.eq.7) then
         ia=ia - 3*mode4
         ib=ib - 3*mode4
      endif
      i0=nint(1270.46/df)
      lag1=-5
      lag2=59
      syncbest=-1.e30
      syncbest2=-1.e30

      call zero(ccfred,901)

      do i=ia,ib
         call xcor24(s2,i,nsteps,nsym,lag1,lag2,mode,mode4,
     +        ccfblue,ccf0,lagpkx,flip)
         j=i-i0
         if(mode.eq.7) j=j + 3*mode4
         if(j.ge.-372 .and. j.le.372) ccfred(j)=ccf0

         !Find rms of the CCF, without the main peak

         call slope(ccfblue(lag1),lag2-lag1+1,lagpk0-lag1+1.0)
         !sync=abs(ccfblue(lagpk0))
         ppmax=psavg(i)-1.0


         if(ppmax.gt.0.2938) then

            lagpk=lagpkx
            ipk=i

            !Peak up in frequency to fraction of channel
            dx=0.
            dfx=(ipk+dx-i0)*df
            if(mode.eq.7) dfx=dfx + 3*mode4*df

            !Peak up in time, at best whole-channel frequency
            call xcor24(s2,ipk,nsteps,nsym,lag1,lag2,mode,mode4,
     +                  ccfblue,ccfmax,lagpk,flip)
            xlag=lagpk
            if(lagpk.gt.lag1 .and. lagpk.lt.lag2) then
               call peakup(ccfblue(lagpk-1),ccfmax,ccfblue(lagpk+1),dx2)
               xlag=lagpk+dx2
            endif

            !Find rms of the CCF, without the main peak
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
            istart=xlag*nq
            dtx=istart*dt
            snrx=-99.0
            ppmax=psavg(ipk)-1.0
            if(ppmax.gt.0.0001) then
               snrx=db(ppmax*df/2500.0) + 7.5        !Empirical
               if(mode.eq.7) snrx=snrx + 3.0         !Empirical
            endif

            nsync=nint(snrsync-3.0)
            nsnr=nint(snrx)

            if(snrx.lt.-33.0) snrx=-33.0
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

 999  return
      end

