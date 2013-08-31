C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine getpfx1(callsign,k)

      character callsign*12
      character*8 c
      character addpfx*8
C     Can't 'include' *.f90 in *.f
C     common/gcom4/addpfx
      include 'pfx.f'

C     jt65-hf will not use addpfx so set it nil here
      addpfx='        '

      iz=index(callsign,' ') - 1
      if(iz.lt.0) iz=12
      islash=index(callsign(1:iz),'/')
      k=0
      c='   '
      if(islash.gt.0 .and. islash.le.(iz-4)) then
!     Add-on prefix
         c=callsign(1:islash-1)
         callsign=callsign(islash+1:iz)

C callsign is the 'root' call.  i.e KC4/W4CQZ would return W4CQZ as callsign
C c is the prefix.  i.e. KC4/W4CQZ would return KC4 as c

C NZ is defined in pfx.f (currently = 339)
C pfx() is defined in pfx.f
C look to see that c (prefix) is defined.

         do i=1,NZ
            if(pfx(i)(1:4).eq.c) then
               k=i
               go to 10
            endif
         enddo
C if the prefix isn't in pfx() then k will be 0 here
C if prefix found k will be the index of prefix in pfx()

C jt65-hf will not use the addpfx mechanism.

!         if(addpfx.eq.c) then
!            k=449
!            go to 10
!         endif

C having made it here there is either no prefix present or it is one not defined
C in pfx().  Check first for suffix.

      else if(islash.eq.(iz-1)) then
!     Add-on suffix
         c=callsign(islash+1:iz)
         callsign=callsign(1:islash-1)

C callsign is the 'root' call.  i.e. W4CQZ/6 would return W4CQZ as callsign
C c is the suffix.  i.e. W4CQZ/6 would return 6 as c

C NZ2 is defined in pfx.f (currently = 12)
C sfx() is defined in pfx.f
C look to see that c (suffix) is defined.

         do i=1,NZ2
            if(sfx(i).eq.c(1:1)) then
               k=400+i
               go to 10
            endif
         enddo
C if the suffix isn't in sfx() then k will be 0 here
C if suffix found k will be the index of suffix in sfx()

C having made it here there is either no prefix present (or no valid prefix) and no
C suffix present (or valid suffix) so k will = 0.

      endif

 10   if(islash.ne.0 .and.k.eq.0) k=-1

C k = -1 = no valid prefix or suffix found even though one was present in the callsign
C block.  So... k = -1 can be considered an error condition. k = 0 = no prefix/suffix
C in callsign.  k > 0 = prefix or suffix was found and validated. k points to index in
C pfx() or sfx().

      return
      end

