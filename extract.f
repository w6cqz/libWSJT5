      subroutine extract(s3,nadd,ncount,mrsym,mr2sym,mrprob,mr2prob)

      real s3(64,63)
      real tmp(4032)
      integer era(51),indx(64)
      integer mrsym(63),mr2sym(63),mrprob(63),mr2prob(63)
      integer nhist
      integer system
      common/extcom/ntdecode
      data nsec1/0/
      save

C     Clearing the decoded symbol/probability arrays.
      do i=1,63
         mrsym(i)=0
         mr2sym(i)=0
         mrprob(i)=0
         mr2prob(i)=0
      enddo

 1    call demod64a(s3,nadd,mrsym,mrprob,mr2sym,mr2prob,ntest,nlow)

      if(ntest.lt.50 .or. nlow.gt.20) then
         ncount=-999                         !Flag bad data
         go to 900
      endif

C     Most probable decoded symbols
      call graycode(mrsym,63,-1)   ! Removes Gray coding
      call interleave63(mrsym,-1)  ! Deinterleaves symbols
      call interleave63(mrprob,-1) ! Deinterleaves probabilites
C     Next most probable decoded symbols
      call graycode(mr2sym,63,-1)
      call interleave63(mr2sym,-1)
      call interleave63(mr2prob,-1)

 900  return
      end
