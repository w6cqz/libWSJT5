C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine xfft(x,nfft,ical,wisfile)

C  Real-to-complex FFT.

      real x(nfft)

C      call four2(x,nfft,1,-1,0)
C      print *,'Entering xfft(x,nfft,ical)'
C      print *,'call four2a(x,nfft,1,-1,0,ical)'
      call four2a(x,nfft,1,-1,0,ical,wisfile)
C      print *,'Done...'
C      print *,'Leaving xfft(x,nfft,ical)'
      return
      end

