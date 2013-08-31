C      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C      contributions from additional authors.  WSJT is Open Source
C      software, licensed under the GNU General Public License V2 (GPL).
C      Source code and programming information may be found at
C      http://developer.berlios.de/projects/wsjt/.
      subroutine fil651(d,n1,c,n2)

C  FIR lowpass mixing filter designed with ScopeFIR.  Real in, complex out.

C  fsample     = 5512.5   Hz
C  Ntaps       = 31
C  fc          = 1000     Hz
C  fstop       = 1378.125 Hz
C  Ripple      = 0.5      dB
C  Stop Atten  = 50       dB
C  fmix        = 1378.125 Hz
C  fout        = 2706.25  Hz

      parameter (NTAPS=31)
      parameter (NH=NTAPS/2)
      parameter (NDOWN=2)               !Downsample ratio

      real d(n1)
      complex c(n2)

      complex ck(-NH:NH)
      data ck/
     +  (-0.000000073578,-0.000859869243),
     +  ( 0.008518289457,-0.000000680308),
     +  ( 0.000000834309, 0.011250152594),
     +  (-0.001061705254, 0.000000072679),
     +  ( 0.000000875897, 0.013958392128),
     +  (-0.010047338711, 0.000000573160),
     +  ( 0.000000770320, 0.015003869984),
     +  (-0.025027880956, 0.000001142192),
     +  ( 0.000000285583, 0.007151700551),
     +  (-0.043634723888, 0.000001493512),
     +  (-0.000000478847,-0.016788108005),
     +  (-0.061886192046, 0.000001412144),
     +  (-0.000001258694,-0.073548459509),
     +  (-0.075261027462, 0.000000858668),
     +  (-0.000001749252,-0.306638863572),
     +  ( 0.419826269508, 0.000000000000),
     +  (-0.000001749252, 0.306638863572),
     +  (-0.075261027462,-0.000000858668),
     +  (-0.000001258694, 0.073548459509),
     +  (-0.061886192046,-0.000001412144),
     +  (-0.000000478847, 0.016788108005),
     +  (-0.043634723888,-0.000001493512),
     +  ( 0.000000285583,-0.007151700551),
     +  (-0.025027880956,-0.000001142192),
     +  ( 0.000000770320,-0.015003869984),
     +  (-0.010047338711,-0.000000573160),
     +  ( 0.000000875897,-0.013958392128),
     +  (-0.001061705254,-0.000000072679),
     +  ( 0.000000834309,-0.011250152594),
     +  ( 0.008518289457, 0.000000680308),
     +  (-0.000000073578, 0.000859869243)/

      n2=(n1-NTAPS+NDOWN)/NDOWN
      k0=NH-NDOWN+1

      do i=1,n2
         c(i)=0.
         k=k0 + NDOWN*i
         do j=-NH,NH
            c(i)=c(i) + d(j+k)*conjg(ck(j))
         enddo
      enddo

      return
      end
