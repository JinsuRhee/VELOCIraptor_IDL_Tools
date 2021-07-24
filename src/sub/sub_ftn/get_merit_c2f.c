#include <stdio.h>

void get_merit(int argc, void *argv[])
{
  extern void get_merit_();   /* FORTRAN routine */
  int *larr;
  double *darr;
  long long *pid, *pid2;

  larr		= (int *) argv[0];
  darr		= (double *) argv[1];
  pid		= (long long *) argv[2];
  pid2		= (long long *) argv[3];

  get_merit_(larr, darr, pid, pid2);   /* Compute sum */
}
