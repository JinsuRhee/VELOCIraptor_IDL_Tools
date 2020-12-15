#include <stdio.h>

void get_magnitude(int argc, void *argv[])
{
  extern void get_magnitude_();   /* FORTRAN routine */
  double *rr, *xx, *yy, *zz;
  int *bin, *uin;
  double *pos, *flux, *mag;
  int *larr;
  double *darr;

  rr		= (double *) argv[0];
  xx		= (double *) argv[1];
  yy		= (double *) argv[2];
  zz		= (double *) argv[3];
  bin		= (int *) argv[4];
  uin		= (int *) argv[5];
  pos		= (double *) argv[6];
  flux		= (double *) argv[7];
  mag		= (double *) argv[8];
  larr		= (int *) argv[9];
  darr		= (double *) argv[10];

  get_magnitude_(rr, xx, yy, zz, bin, uin, pos, flux, mag, larr, darr);   /* Compute sum */
}
