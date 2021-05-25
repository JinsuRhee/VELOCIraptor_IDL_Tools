#include <stdio.h>
#include <sys/resource.h>

typedef struct {
   unsigned short slen;         /* length of the string         */
   short stype;                 /* Type of string               */
   char *s;                     /* Pointer to chararcter array  */
} STRING;

#define STR_LEN(__str)    ((long)(__str)->slen)

void get_contam(int argc, void *argv[])
{
  extern void get_contam_();   /* FORTRAN routine */
  double *xc, *yc, *zc, *rr;
  int *larr;
  double *darr;
  int *dom_list;
  double *confrac, *conf_r;

  STRING *dir_raw;

  larr		= (int *) argv[0];
  darr		= (double *) argv[1];
  dir_raw	= (STRING *) argv[2];
  xc		= (double *) argv[3];
  yc		= (double *) argv[4];
  zc		= (double *) argv[5];
  rr		= (double *) argv[6];
  dom_list	= (int *) argv[7];
  confrac	= (double *) argv[8];
  conf_r	= (double *) argv[9];


  get_contam_(larr, darr, dir_raw->s, xc, yc, zc, rr, dom_list, confrac, conf_r);   /* Compute sum */}
