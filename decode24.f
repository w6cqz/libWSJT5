      subroutine decode24(dat,npts,dtx,dfx,flip,mode,mode4,
     +  decoded,ncount,qual,ical,wisfile,kvfile)

C  Decodes JT65 data, assuming that DT and DF have already been determined.

      real dat(npts)                        !Raw data
      character decoded*22
      character*72 c72
      real*8 dt,df,phi,f0,dphi,twopi,phi1,dphi1
      complex*16 cz,cz1,c0,c1
      integer*1 symbol(207)
      integer*1 data1(13)                   !Decoded data (8-bit bytes)
      integer   data4a(9)                   !Decoded data (8-bit bytes)
      integer   data4(12)                   !Decoded data (6-bit bytes)
      integer amp,delta
      integer mettab(0:255,0:1)             !Metric table
      integer ical
      character*255 wisfile,kvfile
      integer fano
      integer nch(7)
      integer npr2(207)
      include 'avecom.h'
      data mode0/-999/
      data npr2/
     +  0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,1,1,0,0,
     +  0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,0,1,1,1,1,1,0,1,0,0,0,
     +  1,0,0,1,0,0,1,1,1,1,1,0,0,0,1,0,1,0,0,0,1,1,1,1,0,1,1,0,0,1,
     +  0,0,0,1,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,0,1,0,1,0,1,1,0,1,0,1,
     +  0,1,1,1,0,0,1,0,1,1,0,1,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,1,
     +  0,1,1,1,0,1,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,1,0,0,0,1,1,1,1,1,
     +  1,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,1,0,1,1,1,1,0,1,0,1/
      data nch/1,2,4,9,18,36,72/
      save mettab,mode0

      if(mode.ne.mode0) then
         call genmet(mode,mettab)
      endif

      mode0=mode
      twopi=8*atan(1.d0)
      dt=2.d0/11025             !Sample interval (2x downsampled data)
      df=11025.d0/2520.d0
      nsym=206
      amp=15
      istart=nint(dtx/dt)              !Start index for synced FFTs
      if(istart.lt.0) istart=0
      idfbest = 0
      nchips = 0
      ich = 0

C  Should amp be adjusted according to signal strength?

C  Compute soft symbols using differential BPSK demodulation
      c0=0.                                !### C0=amp ???
      k=istart
      fac=1.e-4
      phi=0.d0
      phi1=0.d0
      ang0=0.

      if(mode.eq.6) then                   !JT2
         nhmax=0
         do idf=-20,20,2
            c0=amp
            k=istart
            phi=0.d0
            nh=0
            do j=1,nsym+1
               if(flip.gt.0.0) then
                  f0=1270.46 + dfx + npr2(j)*df
               else
                  f0=1270.46 + dfx + (1-npr2(j))*df
               endif
               f0=f0 + 0.1*idf
               dphi=twopi*dt*f0
               c1=0.
               do i=1,1260
                  k=k+1
                  phi=phi+dphi
                  cz=dcmplx(cos(phi),-sin(phi))
                  if(k.le.npts) c1=c1 + dat(k)*cz
               enddo
               rsym=amp*(real(c1)*real(c0) + aimag(c1)*aimag(c0))
               ang=atan2(aimag(c1),real(c1))
               ndang=nint(57.1957795131d0*(ang-ang0))
               ang0=ang
               if(ndang.le.-180) ndang=ndang+360
               if(ndang.gt.180) ndang=ndang-360
               if(ndang.lt.-90) ndang=ndang+180
               if(ndang.gt. 90) ndang=ndang-180
               if(rsym.ge.0.05 .and. abs(ndang).lt.20)nh=nh+1
            enddo
            if(nh.gt.nhmax) then
               nhmax=nh
               idfbest=idf
            endif
         enddo

         c0=0
         k=istart
         phi=0.d0
         do j=1,nsym+1
            if(flip.gt.0.0) then
               f0=1270.46 + dfx + npr2(j)*df
            else
               f0=1270.46 + dfx + (1-npr2(j))*df
            endif
            f0=f0 + 0.1*idfbest
            dphi=twopi*dt*f0
            c1=0.
            do i=1,1260
               k=k+1
               phi=phi+dphi
               cz=dcmplx(cos(phi),-sin(phi))
               if(k.le.npts) c1=c1 + dat(k)*cz
            enddo
            c1=fac*c1
            rsym=amp*(real(c1)*real(c0) + aimag(c1)*aimag(c0))

C  NB: It may be possible to track phase.  In that case, remove the 
C  average phase and then use:
!            rsym=amp*real(c1)*real(c0)

            ang=atan2(aimag(c1),real(c1))
            ndang=nint(57.1957795131d0*(ang-ang0))
            if(ndang.le.-180) ndang=ndang+360
            if(ndang.gt.180) ndang=ndang-360
            c0=c1
            r=rsym+128.
            if(r.gt.255.0) r=255.0
            if(r.lt.0.0) r=0.0
            i4=nint(r)
            if(i4.gt.127) i4=i4-256
            if(j.ge.1) symbol(j)=i4
            ang0=ang
         enddo
         go to 50
      endif

! JT4 mode
      ich=0
 40   ich=ich+1
      nchips=nch(ich)
      nspchip=1260/nchips
      k=istart
      phi=0.d0
      phi1=0.d0
      fac2=1.e-8 * sqrt(float(mode4))
      do j=1,nsym+1
         if(flip.gt.0.0) then
            f0=1270.46 + dfx + (npr2(j)-1.5)*mode4*df
            f1=1270.46 + dfx + (2+npr2(j)-1.5)*mode4*df
         else
            f0=1270.46 + dfx + (1-npr2(j)-1.5)*mode4*df
            f1=1270.46 + dfx + (3-npr2(j)-1.5)*mode4*df
         endif
         dphi=twopi*dt*f0
         dphi1=twopi*dt*f1
         sq0=0.
         sq1=0.
         do nc=1,nchips
            phi=0.d0
            phi1=0.d0
            c0=0.
            c1=0.
            do i=1,nspchip
               k=k+1
               phi=phi+dphi
               phi1=phi1+dphi1
               cz=dcmplx(cos(phi),-sin(phi))
               cz1=dcmplx(cos(phi1),-sin(phi1))
               if(k.le.npts) then
                  c0=c0 + dat(k)*cz
                  c1=c1 + dat(k)*cz1
               endif
            enddo
            sq0=sq0 + real(c0)**2 + aimag(c0)**2
            sq1=sq1 + real(c1)**2 + aimag(c1)**2
         enddo
         sq0=fac2*sq0
         sq1=fac2*sq1
         rsym=amp*(sq1-sq0)
         r=rsym+128.
         if(r.gt.255.0) r=255.0
         if(r.lt.0.0) r=0.0
         i4=nint(r)
         if(i4.gt.127) i4=i4-256
         if(j.ge.1) symbol(j)=i4
      enddo

 50   nbits=72+31
      delta=100
      limit=100000
      ncycles=0
      call interleave24(symbol(2),-1)         !Remove the interleaving

C  This is a kludge:
      iters=1
      if(mode.eq.6) iters=2
      do iter=1,iters
         if(iter.eq.2) then
            do i=2,207
               i4=symbol(i)
               if(i4.lt.0) i4=i4+256
               i4=255-i4
               if(i4.gt.127) i4=i4-256
               symbol(i)=i4
            enddo
         endif
         ncount=fano(metric,ncycles,data1,symbol(2),nbits,mettab,
     +        delta,limit)
         if(ncount.ge.0) go to 100
      enddo
      if(mode.eq.7 .and. nchips.lt.mode4) go to 40

 100  do i=1,9
         i4=data1(i)
         if(i4.lt.0) i4=i4+256
         data4a(i)=i4
      enddo
      write(c72,1100) (data4a(i),i=1,9)
 1100 format(9b8.8)
      read(c72,1102) data4
 1102 format(12b6)

      decoded='                      '
      if(ncount.ge.0) call unpackmsg(data4,decoded)
      if(decoded(1:6).eq.'000AAA') then
         decoded='***WRONG MODE?***'
         ncount=-1
      endif

      qual=0.

C  Save symbol spectra for possible decoding of average.

      return
      end
