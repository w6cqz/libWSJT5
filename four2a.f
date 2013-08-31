      SUBROUTINE FOUR2a (a,nfft,NDIM,ISIGN,IFORM,ical,wisfile)
C
C     IFORM = 1, 0 or -1, as data is
C     complex, real, or the first half of a complex array.  Transform
C     values are returned in array DATA.  They are complex, real, or
C     the first half of a complex array, as IFORM = 1, -1 or 0.
C
C     The transform of a real array (IFORM = 0) dimensioned N(1) by N(2)
C     by ... will be returned in the same array, now considered to
C     be complex of dimensions N(1)/2+1 by N(2) by ....  Note that if
C     IFORM = 0 or -1, N(1) must be even, and enough room must be
C     reserved.  The missing values may be obtained by complex conjuga-
C     tion.
C
C     The reverse transformation of a half complex array dimensioned
C     N(1)/2+1 by N(2) by ..., is accomplished by setting IFORM
C     to -1.  In the N array, N(1) must be the true N(1), not N(1)/2+1.
C     The transform will be real and returned to the input array.
      parameter (NPMAX=100)
      complex a(nfft)
      complex aa(32768)
      integer nn(NPMAX),ns(NPMAX),nf(NPMAX),nl(NPMAX)
      integer plan(NPMAX)                 !As recommended by fftw3 docs
      integer ierr
      character*255 wisfile
      data nplan/0/
      include 'fftw3.f'
      save
      if(nfft.lt.0) go to 999
      nloc=loc(a)
      do i=1,nplan
         if(nfft.eq.nn(i) .and. isign.eq.ns(i) .and.
     +      iform.eq.nf(i) .and. nloc.eq.nl(i)) go to 10
      enddo
      if(nplan.ge.NPMAX) stop 'Too many FFTW plans requested.'

      nplan=nplan+1
      i=nplan
      nn(i)=nfft
      ns(i)=isign
      nf(i)=iform
      nl(i)=nloc

C
C     ical =  0 = FFTW_ESTIMATE set, no load/no save wisdom.
C     ical =  1 = FFTW_MEASURE set, yes load/no save wisdom.
C     ical =  2 = FFTW_PATIENT set, yes load/no save wisdom.
C     ical = 11 = FFTW_MEASURE set, no load/no save wisdom.
C     ical = 12 = FFTW_PATIENT set, no load/no save wisdom.
C     ical = 21 = FFTW_MEASURE set, no load/yes save wisdom.
C     ical = 22 = FFTW_PATIENT set, no load/yes save wisdom.
C
C     Use ical = 21 or 22 to compute the optimal FFT plans and save.
C     Use ical =  1 or  2 to load saved wisdom.
C     Use ical = 11 or 12 when wisdom has been loaded and does not need
C                         saving.
C     Use ical = 0 when all else fails.
C
      if(ical.lt.0 .or. ical.gt.22) ical=0
      if(ical.eq.0) nspeed=FFTW_ESTIMATE
      if(ical.eq.1) nspeed=FFTW_MEASURE
      if(ical.eq.2) nspeed=FFTW_PATIENT
      if(ical.eq.11) nspeed=FFTW_MEASURE
      if(ical.eq.12) nspeed=FFTW_PATIENT
      if(ical.eq.21) nspeed=FFTW_MEASURE
      if(ical.eq.22) nspeed=FFTW_PATIENT

      if(nfft.le.32768) then
         do j=1,nfft
            aa(j)=a(j)
         enddo
      endif

      ierr=2

C      print *,'four2a ical:  ',ical

C      if(ical.eq.1 .or. ical.eq.2) Then
C         print *,'four2a reading wisdom'
C      endif

      if(ical.eq.1 .or. ical.eq.2) Then
C         print *,'Reading wisdom'
         if(ical.eq.1) open(35, FILE=wisfile,
     +      ACCESS='SEQUENTIAL', STATUS='OLD', ERR=444)
         if(ical.eq.2) open(35, FILE=wisfile,
     +      ACCESS='SEQUENTIAL', STATUS='OLD', ERR=444)
         call import_wisdom_from_file(ierr,35)
         close(35)
C        ierr will be FFTW_SUCCESS = 1, FFTW_FAILURE = 0
      endif

      goto 555

 444  ical = ical + 20
C      print *,'Wisdom read error.  Setting ical=',ical

     
 555  if(ical.eq.1 .or. ical.eq.2) Then

C         print *,'ierr:  ',ierr,' != 1 is error.  ical:  ',ical

         if(ierr.ne.1) Then ical=ical+20

C         print *,'ical:  ',ical

C        This forces a save when an error is found with wisdom read.
      endif

      if(isign.eq.-1 .and. iform.eq.1) then

C         print *,'Calling sfftw_plan_dft_1d(plan(',i,'),',nfft,
C     +           ' FFTW_FORWARD,',nspeed

         call sfftw_plan_dft_1d(plan(i),nfft,a,a,
     +        FFTW_FORWARD,nspeed)
      else if(isign.eq.1 .and. iform.eq.1) then

C         print *,'Calling sfftw_plan_dft_1d(plan(',i,'),',nfft,
C     +           ' FFTW_BACKWARD,',nspeed

         call sfftw_plan_dft_1d(plan(i),nfft,a,a,
     +        FFTW_BACKWARD,nspeed)
      else if(isign.eq.-1 .and. iform.eq.0) then

C         print *,'Calling sfftw_plan_dft_r2c_1d(plan(',i,'),',nfft,
C     +           ',',nspeed

         call sfftw_plan_dft_r2c_1d(plan(i),nfft,a,a,nspeed)
      else if(isign.eq.1 .and. iform.eq.-1) then

C         print *,'Calling sfftw_plan_dft_c2r_1d(plan(',i,'),',nfft,
C     +           ',',nspeed

         call sfftw_plan_dft_c2r_1d(plan(i),nfft,a,a,nspeed)
      else
      endif
      i=nplan
      if(nfft.le.32768) then
         do j=1,nfft
            a(j)=aa(j)
         enddo
      endif

C      print *,'Calling sfftw_execute(plan(',i,'))'

 10    call sfftw_execute(plan(i))


      if(ical.eq.21 .or. ical.eq.22) Then
C         print *,'four2a writing wisdom'
         if(ical.eq.21) open(35, FILE=wisfile,
     +      ACCESS='SEQUENTIAL', STATUS='REPLACE')
         if(ical.eq.22) open(35, FILE=wisfile,
     +      ACCESS='SEQUENTIAL', STATUS='REPLACE')
         call export_wisdom_to_file(35)
         close(35)
      endif

C      print *,'Leaving four2a'

      return

 999  do i=1,nplan
C         print *,'Calling sfftw_destroy_plan(plan(',i,'))'
         call sfftw_destroy_plan(plan(i))
      enddo
      
C      print *,'Leaving four2a'

      return
      end
