      subroutine g65(txdf,tsyms,shmsg,iwave,nwave,level)

C     Encodes 63 Six bit channel symbols into a wavefile.

      parameter (NMAX=60*11025)     !Max length of wave file

      real*8 t,dt,phi,f,f0,dfgen,dphi,twopi,tsymbol,glevel

      integer*2 iwave(NMAX)  !Generated wave file
      integer txdf
      integer level
      integer tsyms(63)
      integer shmsg
      integer nwave
      include 'prcom.h'

      data twopi/6.283185307d0/

      nwave=0

      glevel = level * 1.0

      if(level.gt.32767) glevel = 32767.0

      call setup65 ! Setup sync vector 

C     If shmsg> 0 it is a SH message (1-ATT, 2-RO, 3-RRR, 4-73)

      if(shmsg.eq.0) then
         tsymbol=4096.d0/11025.d0
         nsym=126                          !Symbols per transmission
      else
         tsymbol=16384.d0/11025.d0
         nsym=32
      endif

C     Set up necessary constants
      dt=1.0/11025.0
      f0=118*11025.d0/1024 + txdf ! ~1270.5 + txdf
      dfgen=11025.0/4096.0
      t=0.d0
      phi=0.d0
      k=0
      j0=0
      ndata=(nsym*11025.d0*tsymbol)/2
      ndata=2*ndata
      do i=1,ndata
         t=t+dt
         j=int(t/tsymbol) + 1 !Symbol number, 1-126
         if(j.ne.j0) then
            f=f0
            if(shmsg.ne.0 .and. mod(j,2).eq.0) f=f0+10*shmsg*dfgen
            if(shmsg.eq.0 .and. pr(j).lt.0.0) then
               k=k+1
               f=f0+(tsyms(k)+2)*dfgen
            endif
            dphi=twopi*dt*f
            j0=j
         endif
         phi=phi+dphi
         iwave(i)=glevel*sin(phi)
      enddo

      nwave=i !nwave is the last index value of last sample

      return
      end

