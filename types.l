#include <stdio.h>
#include <setjmp.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

typedef char *PCHAR;
#define myalloc(x)   malloc(x)

#define   DZ         8                    /* # of decimal digits in one cell */
#define   BASE      100000000L            /* 10**DZ */
#define   BM1        99999999L          /* 10**DZ -1 */
#define   BFT     10000000000000000.0     /* 10**(2*DZ)  */

#define   BN         2147483647L          /* biggest positive # on long int type */
/* completely machine dependent PAGESIZE. use 'size.c' in order
   to find this number.
   rule: 'product of all sizes' mod PAGESIZE should be zero. */

#define PAGESIZE    12000
#define TXSIZE       121

/* big integer arithmetic machine specific constants */

/* 9223372036854775810    (64 bits) number for 10 byte double */
/* 4503599627370497       (53 bits) number for 8 byte double */

typedef int *PSEXP, **PPSEXP;

#if BITF
struct Xid {
	char Xtype;
	unsigned Xisinheap : 1;
	unsigned Xisglobal : 1;
	unsigned Xisfluid  : 1;
	unsigned Xisfunction : 1;
	unsigned Xisinoblist : 1;
	unsigned Xisdclfluid : 1;
	struct Xid *Xhashlink;
	PSEXP Xvalue;
	PSEXP Xproplist;
	char *Xpname;
};
#else
struct Xid {
	char Xtype;
	char Xattr;
	struct Xid *Xhashlink;
	PSEXP Xvalue;
	PSEXP Xproplist;
	char *Xpname;
};
#endif

struct Xpair {
	char Xtype;
	PSEXP Xcar;
	PSEXP Xcdr;
};

struct Xstring {
	char Xtype;                       /* Added in 7.0 */
	char *Xstrelement;
};

typedef struct Xstring STRING, *PSTRING;

struct Xinteger {
	char Xtype;
	long int Xintval;
};
struct Xbig {
	char Xtype;
	long int Xintval;
	PSEXP Xcdr;
};
struct Xfloating {
	char Xtype;
	double Xfloval;
};
struct Xvector {
	char Xtype;
	int Xupbv;
	PSEXP *Xvectelts;
	char Xused;
};
struct Xfpointer {
	char Xtype;
	char Xargno;
	void (*Xfnc)();
};
struct Xerrmsg {
	char Xtype;
	int Xerrorno;
	PSEXP Xerrormsg;
};

struct Xforwardadr {
	char Xtype;                                   /*v 6.0  *GU*/
	PSEXP Xforwardaddress;
};                                                    /*v 6.0  *GU*/

typedef struct Xid ID, *PID;
typedef struct Xpair PAIR, *PPAIR;
typedef struct Xinteger INTEGER, *PINTEGER;
typedef struct Xbig BIG, *PBIG;
typedef struct Xfloating FLOATING, *PFLOATING;
typedef struct Xvector VECTOR, *PVECTOR;
typedef struct Xfpointer FPOINTER, *PFPOINTER;
typedef struct Xerrmsg ERRMSG, *PERRMSG;
typedef struct Xforwardadr FORWARDADR, *PFORWARDADR;    /*v 6.0  *GU*/

#define Tpair        0
#define Tid          1
#define Tstring      2
#define Tinteger     3
#define Tbig         4
#define Tfloating    5
#define Tvector      6
#define Tfpointer    7
#define Terrmsg      8
#define Tpname       9
#define Tsname      10
#define Tforwardadr 11
#define Tuser       12

#define Sexp(x)       ((PSEXP) (x))

#define T             (Sexp(&chrid['t']))
#define NIL           (Sexp(&nil))
