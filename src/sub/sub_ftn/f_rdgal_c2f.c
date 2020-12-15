#include <stdio.h>

typedef struct {
   unsigned short slen;         /* length of the string         */
   short stype;                 /* Type of string               */
   char *s;                     /* Pointer to chararcter array  */
} STRING;

#define STR_LEN(__str)    ((long)(__str)->slen)

void f_rdgal(int argc, void *argv[])
{
  extern void f_rdgal_();   /* FORTRAN routine */
  STRING *dir; 
  int *larr;
  double *darr;

  larr		= (int *) argv[0];
  darr		= (double *) argv[1];
  dir		= (STRING *) argv[2];

  //printf("%s", dir->s);

  f_rdgal_(larr, darr, dir->s);   /* Compute sum */
}
