C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine gen65(message,ntxdf,iwave,nwave,
     +  sendingsh,msgsent,nmsg)

C     Encodes a JT65 message into a wavefile.

      parameter (NMAX=60*11025)     !Max length of wave file
      character*22 message          !Message to be generated
      character*22 msgsent          !Message as it will be received
      character*3 cok               !'   ' or 'OOO'
      character*6 c1,c2
      real*8 t,dt,phi,f,f0,dfgen,dphi,twopi,samfac,tsymbol

      integer*2 iwave(NMAX)  !Generated wave file
      integer dgen(12)
      integer sent(63)
      integer sendingsh
      character addpfx*8     !Add-on prefix, as in ZA/PA2CHR
      character pfx*8
      common/gcom4/addpfx
      common/c1c2/c1,c2
      include 'prcom.h'
      data twopi/6.283185307d0/
      save

      addpfx=pfx
C     Setting mode65=1 and samfac=1.0 implicitly.
      mode65=1
      samfac=1.0
      if(abs(pr(1)).ne.1.0) call setup65
C
C     Need to modify this such that I never ever set flip and only
C     set shorthand flag when I really mean to send shorthand.  As it
C     is now if I enter a free text message starting with RRR,RO or 73
C     it is sent shorthand even if there's text after the SH characters.
C
C Doing away with chkmsg.  It's just as easy to deal with this check
C here and makes the code easier to follow.
C     Setting flip off and cok Nil
      flip = 1.0
      cok = '   '
      nspecial = 0
C Will want to modify this to allow ATT, RO, RRR and 73 to be parts of
C a free text message.  In other words, look beyond message(1:2 or 3)
C to see if there is more.  :)  I'm also considering the implication of
C enabling ATT SH msg.  It might have some use on HF.
!      if(message(1:3).eq.'ATT') nspecial=1
      if(message(1:2).eq.'RO')  nspecial=2
      if(message(1:3).eq.'RRR') nspecial=3
      if(message(1:2).eq.'73')  nspecial=4

      if(nspecial.eq.0) then
C        If nspecial > 0 it is a SH message (1-ATT, 2-RO, 3-RRR, 4-73)
         call packmsg(message,dgen)        !Pack message into 72 bits
         sendingsh=0
         if(iand(dgen(10),8).ne.0) sendingsh=-1   !Plain text flag
         call rs_encode(dgen,sent)
         call interleave63(sent,1)         !Apply interleaving
         call graycode(sent,63,1)          !Apply Gray code
         tsymbol=4096.d0/11025.d0
         nsym=126                          !Symbols per transmission
      else
         tsymbol=16384.d0/11025.d0
         nsym=32
         sendingsh=1                       !Flag for shorthand message
      endif

C     Set up necessary constants
      dt=1.0/(samfac*11025.0)
      f0=118*11025.d0/1024 + ntxdf
      dfgen=mode65*11025.0/4096.0
      t=0.d0
      phi=0.d0
      k=0
      j0=0
      ndata=(nsym*11025.d0*samfac*tsymbol)/2
      ndata=2*ndata
      do i=1,ndata
         t=t+dt
         j=int(t/tsymbol) + 1              !Symbol number, 1-126
         if(j.ne.j0) then
            f=f0
            if(nspecial.ne.0 .and. mod(j,2).eq.0) f=f0+10*nspecial*dfgen
            if(nspecial.eq.0 .and. flip*pr(j).lt.0.0) then
               k=k+1
               f=f0+(sent(k)+2)*dfgen
            endif
            dphi=twopi*dt*f
            j0=j
         endif
         phi=phi+dphi
         iwave(i)=32767.0*sin(phi)
      enddo

      do j=1,5512             !Put another 0.5 sec of silence at end
         i=i+1
         iwave(i)=0
      enddo
      nwave=i
      call unpackmsg(dgen,msgsent)
      if(flip.lt.0.0) then
         do i=22,1,-1
            if(msgsent(i:i).ne.' ') goto 10
         enddo
 10      msgsent=msgsent(1:i)//' OOO'
      endif
      do i=22,1,-1
         if(msgsent(i:i).ne.' ') goto 20
      enddo
 20   nmsg=i

      return
      end

