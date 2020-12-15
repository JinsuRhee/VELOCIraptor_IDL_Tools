#include <stdio.h>

void prop_sfr(int argc, void *argv[])
{
  extern void prop_sfr_();   /* FORTRAN routine */
  double *xc, *yc, *zc, *rr;
  int *bin, *uin;
  double *pos, *mass, *gyr;
  double *sfr;
  int *larr;
  double *darr;

  xc		= (double *) argv[0];
  yc	 	= (double *) argv[1];
  zc		= (double *) argv[2];
  rr		= (double *) argv[3];
  bin		= (int *) argv[4];
  uin		= (int *) argv[5];
  pos		= (double *) argv[6];
  mass		= (double *) argv[7];
  gyr		= (double *) argv[8];
  sfr		= (double *) argv[9];
  larr		= (int *) argv[10];
  darr		= (double *) argv[11];

  prop_sfr_(xc, yc, zc, rr, bin, uin, pos, mass, gyr, sfr, larr, darr);   /* Compute sum */
}
