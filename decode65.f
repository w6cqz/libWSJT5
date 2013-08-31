C     Modified by W6CQZ (c) 2012
C     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C     contributions from additional authors.  WSJT is Open Source
C     software, licensed under the GNU General Public License V2 (GPL).
C     Source code and programming information may be found at
C     http://developer.berlios.de/projects/wsjt/.
      subroutine decode65(dat,npts,dtx,dfx,flip,ncount,ical,wisfile,
     +                    dsym1,dsym2,dsym1p,dsym2p)

C     Decodes JT65 data, assuming that DT and DF have already been
C     determined.

      real dat(npts)                        !Raw data
      real s2(77,126)
      real s3(64,63)
      real ftrack(126)
      character*255 wisfile
      character*255 kvfile
      include 'avecom.h'
      include 'prcom.h'
      save

      nafc=1

      dt=2.0/11025.0             !Sample interval (2x downsampled data)
      istart=nint(dtx/dt)        !Start index for synced FFTs
      nsym=126

C     Compute spectra of the channel symbols
      f0=1270.46 + dfx
      call spec2d65(dat,npts,nsym,flip,istart,f0,ftrack,nafc,s2,ical,
     +              wisfile)

      do j=1,63
         k=mdat(j)                       !Points to data symbol
         if(flip.lt.0.0) k=mdat2(j)
         do i=1,64
            s3(i,j)=s2(i+7,k)
         enddo
      enddo

      nadd=1

      call extract(s3,nadd,ncount,dsym1,dsym2,dsym1p,dsym2p)

      return
      end
