/*
      Copyright (c) 2001-2009 by Joseph H. Taylor, Jr., K1JT, with
      contributions from additional authors.  WSJT is Open Source
      software, licensed under the GNU General Public License V2 (GPL).
      Source code and programming information may be found at
      http://developer.berlios.de/projects/wsjt/.
 */
int igray_(int *n0, int *idir)
{
  int n;
  unsigned long sh;
  unsigned long nn;
  n=*n0;

  if(*idir>0) return (n ^ (n >> 1));

  sh = 1;
  nn = (n >> sh);
  while (nn > 0) {
    n ^= nn;
    sh <<= 1;
    nn = (n >> sh);
  }
  return (n);
}
