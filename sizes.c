#include <stdio.h>
#include "flags.l"
#if LATTICE
#define void int
#endif

typedef int *PSEXP, **PPSEXP;

#if BITF
 struct Xid { char Xtype;
             int Xisinheap : 1;
             int Xisglobal : 1;
             int Xisfluid  : 1;
             int Xisfunction : 1;
             int Xisinoblist : 1;
             int Xisdclfluid : 1;
             struct Xid *Xhashlink;
             PSEXP Xvalue;
             PSEXP Xproplist;
             char *Xpname; } ;
#else
 struct Xid { char Xtype;
             char Xattr;
             struct Xid *Xhashlink;
             PSEXP Xvalue;
             PSEXP Xproplist;
             char *Xpname; } ;
#endif

struct Xpair { char Xtype;
               PSEXP Xcar;
               PSEXP Xcdr; } ;

struct Xstring { char Xtype;
                 struct Xstrelmnt *Xstrbody; } ;       /*Changed in ver 3.3*/

typedef struct Xstring STRING, *PSTRING;

struct Xstrelmnt { char Xlength;                           /*New in ver 3.3*/
                   PSTRING Xbackpointer;
                   char Xrealstr[254]; } ;

struct Xinteger { char Xtype;
                  long int Xintval; } ;
struct Xbig { char Xtype;
              long int Xintval;
              PSEXP Xcdr; } ;
struct Xfloating { char Xtype;
                   double Xfloval; } ;
struct Xvector { char Xtype;
                 int Xupbv;
                 PSEXP *Xvectelts;
                 char Xused; } ;
struct Xfpointer { char Xtype;
                   char Xargno;
                   int (*Xfnc)(); } ;
struct Xerrmsg { char Xtype;
                 int Xerrorno;
                 PSEXP Xerrormsg; } ;



typedef struct Xid ID, *PID;
typedef struct Xpair PAIR, *PPAIR;
typedef struct Xinteger INTEGER, *PINTEGER;
typedef struct Xbig BIG, *PBIG;
typedef struct Xfloating FLOATING, *PFLOATING;
typedef struct Xvector VECTOR, *PVECTOR;
typedef struct Xfpointer FPOINTER, *PFPOINTER;
typedef struct Xerrmsg ERRMSG, *PERRMSG;
typedef struct Xstrelmnt STRELEMENT, *PSTRELEMENT;


unsigned sz[12] =     { sizeof(PAIR),
			sizeof(ID),
			sizeof(STRING),
			sizeof(INTEGER),
			sizeof(BIG),
			sizeof(FLOATING),
			sizeof(VECTOR),
			sizeof(char),
			sizeof(int),
			sizeof(long),
			sizeof(PSEXP),
			sizeof(double) };

long gcd(long m,long n)
{
 if (n==0) return m;
 if (n>m) return gcd(n,m);
 return gcd(n,m%n); }


void main()
{ int i;
  long j=1;

  printf(" size of PAIR           %d\n",sz[0]);
  printf(" size of ID             %d\n",sz[1]);
  printf(" size of STRING         %d\n",sz[2]);
  printf(" size of INTEGER        %d\n",sz[3]);
  printf(" size of BIG            %d\n",sz[4]);
  printf(" size of FLOATING       %d\n",sz[5]);
  printf(" size of VECTOR         %d\n",sz[6]);
  printf("--------------------------\n");
  printf(" size of char           %d\n",sz[7]);
  printf(" size of int            %d\n",sz[8]);
  printf(" size of long int       %d\n",sz[9]);
  printf(" size of PSEXP          %d\n",sz[10]);
  printf(" size of double         %d\n",sz[11]);
  for(i=0; i<8; i++)
     j = (j/gcd(j,(long) sz[i]))*sz[i];   /* LCM of all sz[i] */
  printf(" Set PAGESIZE to n positive integer multiple of:  %d\n",j);
/*    if(j%sz[i]) j *= sz[i];
  printf(" Set PAGESIZE to an integer multiple of:  %d\n",j); */
}
