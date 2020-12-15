#include <stdio.h>

void f_rdgas(int argc, void *argv[])
{
  extern void f_rdgas_();   /* FORTRAN routine */
  int *larr;
  double *darr, *mass;
  double *dx, *x, *y, *z, *val, *map, *xr, *yr;

  larr		= (int *) argv[0];
  darr		= (double *) argv[1];
  dx		= (double *) argv[2];
  x		= (double *) argv[3];
  y		= (double *) argv[4];
  z		= (double *) argv[5];
  mass		= (double *) argv[6];
  val		= (double *) argv[7];
  map		= (double *) argv[8];
  xr		= (double *) argv[9];
  yr		= (double *) argv[10];


  f_rdgas_(larr, darr, dx, x, y, z, mass, val, map, xr, yr);   /* Compute sum */
}
