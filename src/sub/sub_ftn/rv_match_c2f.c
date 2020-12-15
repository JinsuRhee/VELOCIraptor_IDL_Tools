#include <stdio.h>
#include <sys/resource.h>

typedef struct {
   unsigned short slen;         /* length of the string         */
   short stype;                 /* Type of string               */
   char *s;                     /* Pointer to chararcter array  */
} STRING;

#define STR_LEN(__str)    ((long)(__str)->slen)

void rv_match(int argc, void *argv[])
{
  extern void rv_match_();   /* FORTRAN routine */
  long long *id;
  long *ind_b, *ind_u;
  double *xp, *vp, *zp, *ap, *mp;
  long *dom_list;
  float *rate;

  STRING *dir_raw;
  int *larr;
  double *darr;

  larr		= (int *) argv[0];
  darr		= (double *) argv[1];
  dir_raw	= (STRING *) argv[2];
  id		= (long long *) argv[3];
  ind_b		= (long *) argv[4];
  ind_u		= (long *) argv[5];
  xp		= (double *) argv[6];
  vp		= (double *) argv[7];
  zp		= (double *) argv[8];
  ap		= (double *) argv[9];
  mp		= (double *) argv[10];
  rate		= (float *) argv[11];
  dom_list	= (long *) argv[12];

  //const rlim_t kStackSize = 64L * 1024L * 1024L;   // min stack size = 64 Mb
  //struct rlimit rl;
  //int result;

  //printf("%s",dir_raw->s);
  //result = getrlimit(RLIMIT_STACK, &rl);
  //if (result == 0)
  //{
  //    if (rl.rlim_cur < kStackSize)
  //    {
  //        rl.rlim_cur = kStackSize;
  //        result = setrlimit(RLIMIT_STACK, &rl);
  //        if (result != 0)
  //        {
  //            fprintf(stderr, "setrlimit returned result = %d\n", result);
  //        }
  //    }
  //}


  rv_match_(larr, darr, dir_raw->s, id, ind_b, ind_u, xp, vp, zp, ap, mp, rate, dom_list);   /* Compute sum */}
