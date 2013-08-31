C     Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
C     contributions from additional authors.  WSJT is Open Source
C     software, licensed under the GNU General Public License V2 (GPL).
C     Source code and programming information may be found at
C     http://developer.berlios.de/projects/wsjt/.
      subroutine fil653(c1,n1,c2,n2)

C     FIR lowpass filter designed using ScopeFIR

C     fsample     = 1378.125   Hz
C     Ntaps       = 45
C     fc          = 100        Hz
C     fstop       = 172.265625 Hz
C     Ripple      = 0.5        dB
C     Stop Atten  = 50         dB
C     fout        = 172.265625 Hz
C     BW          = 200        Hz

      parameter (NTAPS=45)
      parameter (NH=NTAPS/2)
      parameter (NDOWN=4)                !Downsample ratio
      complex c1(n1)
      complex c2(n2)

C     Filter coefficients:
      real a(-NH:NH)
      data a/
     +  -0.000005569862,-0.002503777832,-0.004040335617,-0.005717910288,
     +  -0.006153385485,-0.004446125293,-0.000305215272, 0.005557289511,
     +   0.011329120672, 0.014496551280, 0.012703875898, 0.004837591829,
     +  -0.008060363689,-0.022474422302,-0.032964876083,-0.033575486327,
     +  -0.019743889907, 0.009895672340, 0.052467109908, 0.101031155027,
     +   0.146073001698, 0.177927966814, 0.189427119395, 0.177927966814,
     +   0.146073001698, 0.101031155027, 0.052467109908, 0.009895672340,
     +  -0.019743889907,-0.033575486327,-0.032964876083,-0.022474422302,
     +  -0.008060363689, 0.004837591829, 0.012703875898, 0.014496551280,
     +   0.011329120672, 0.005557289511,-0.000305215272,-0.004446125293,
     +  -0.006153385485,-0.005717910288,-0.004040335617,-0.002503777832,
     +  -0.000005569862/

      n2=(n1-NTAPS+NDOWN)/NDOWN
      k0=NH-NDOWN+1

C     Loop over all output samples
      do i=1,n2
         c2(i)=0.
         k=k0 + NDOWN*i
         do j=-NH,NH
            c2(i)=c2(i) + c1(j+k)*a(j)
         enddo
      enddo

      return
      end
