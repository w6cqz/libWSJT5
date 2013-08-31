C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine setup65

C     Defines arrays related to the pseudo-random synchronizing pattern.
C     Executed at program start.

      integer npra(135),nprc(126)
      include 'prcom.h'

C     JT44
      data npra/
     +    1,1,1,0,1,0,0,0,0,1,1,1,0,0,1,1,0,0,0,0,
     +    1,0,0,1,0,0,0,1,0,1,0,1,1,1,0,1,0,1,1,1,
     +    1,0,0,1,0,0,1,0,1,1,1,0,0,1,1,1,0,0,0,0,
     +    0,0,1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,1,0,
     +    1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,0,1,0,1,0,
     +    1,1,1,1,1,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,
     +    1,1,1,0,1,1,0,1,1,0,1,0,1,1,0/

C     JT65
      data nprc/
     + 1,0,0,1,1,0,0,0,1,1,1,1,1,1,0,1,0,1,0,0,
     + 0,1,0,1,1,0,0,1,0,0,0,1,1,1,0,0,1,1,1,1,
     + 0,1,1,0,1,1,1,1,0,0,0,1,1,0,1,0,1,0,1,1,
     + 0,0,1,1,0,1,0,1,0,1,0,0,1,0,0,0,0,0,0,1,
     + 1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,1,0,1,
     + 0,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,0,1,1,
     + 1,1,1,1,1,1/
      data mr2/0/                !Silence g77 warning

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
               if (pr(m-n).gt.0.0) go to 10
            endif
         enddo
         go to 12
 10      mref(k,1)=m-n
 12      mref(k,2)=mr2
         do n=1,10                     !Get ref symbol after data
            if((m+n).le.nsym) then
               if (pr(m+n).gt.0.0) go to 20
            endif
         enddo
         go to 22
 20      mref(k,2)=m+n
 22   continue
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
               if (pr(m-n).lt.0.0) go to 110
            endif
         enddo
         go to 112
 110      mref2(k,1)=m-n
 112      mref2(k,2)=mr2
         do n=1,10
            if((m+n).le.nsym) then
               if (pr(m+n).lt.0.0) go to 120
            endif
         enddo
         go to 122
 120     mref2(k,2)=m+n
 122  continue
      enddo

      return
      end
