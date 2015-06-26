#include <stdio.h>
#include <string.h>

#include "flags.l"
#include "crfile.h"

char ln[132],*buf,*obuf,fn[4],*lnp;
FILE *in_file, *out_file;
char nm[32], pnm[32], val[32],tp[30],compn[30],compc[30],compx[30],argv1[30];
int file_no, j, i1, i2, no;
#if BITF
int x1,x2,x3,x4,x5,x6;
#else
int x1;
#endif

void filecopy()
{ int c;

  while((c = getc(in_file)) != EOF) putc(c, out_file);
  fprintf(out_file,"\n");
  fclose(in_file);
}

void file_open(file_name)
char *file_name;
{ in_file = fopen(file_name, "r");
  if(in_file == NULL)
    { printf("\n File not found: ");
      printf(file_name);
      printf("\n");
      exit(1); }
  setvbuf(in_file,buf,_IOFBF,16000);
}

void lispc()
{
  strcpy(ln,"lispc");
  strcat(ln,fn);
  strcat(ln,".c");
  out_file = fopen(ln,"w");
  if(obuf != NULL) setvbuf(out_file,obuf,_IOFBF,20000);

/*  FLAGS  */
  file_open(flags);
  filecopy();

/*  TYPES AND MACROS  */
  file_open(types);
  filecopy();
  file_open(type);
  filecopy();

  fprintf(out_file,"extern PSEXP urwelt[];\n");

/*  HEADER DECLARATIONS    */

  file_open(header);
  while(fgets(ln, 121, in_file) != NULL)
    { lnp = ln;
      if(ln[0] == '\n' || ln[0] == 'e' || ln[0] == '#')
        { fprintf(out_file,ln); continue; }
      if(ln[0] == ' ' || ln[0] == '\t') continue;
      while(1)
        { if(*lnp == ';') { *(++lnp) = 0; break; }
          if(*lnp == '=') { *lnp-- = 0; *lnp = ';'; break; }
          lnp++;
        }
      fprintf(out_file,"extern %s\n", ln);
    }
  fclose(in_file);


/*  SYSTEM IDENTIFIERS  */

  file_open(sysids);
  fprintf(out_file,"\n");
#if BITF
  while(fscanf(in_file,"%s%d%d%d%d%d%d%s%s",
                             nm,&x1,&x2,&x3,&x4,&x5,&x6,pnm,val) != EOF)
#else
  while(fscanf(in_file, "%s%d%s%s", nm, &x1, pnm, val) != EOF)
#endif
     fprintf(out_file,"extern ID %s; \n", nm);
  fclose(in_file);

/* CHARACTER IDENTIFIERS  */

  fprintf(out_file,"\nextern ID chrid[128];\n");

/* FUNCTION NAME IDENTIFIERS */

  fprintf(out_file,"\nextern ID fname\[\];\n");


/*  FUNCTION DECLARATIONS  */

  strcpy(ln,compn);
  strcat(ln,fn);
 /* strcat(ln,"."); */
  file_open(ln);
  fprintf(out_file,"\nvoid ");
  j = 0;
  while(fscanf(in_file,  "%s%s%d%s", nm, pnm, &no, tp) != EOF)
    { if(j != 0) fprintf(out_file,",");
      if(j%6 == 0) fprintf(out_file,"\n    ");
      strcat(pnm, "()");
      fprintf(out_file," %s", pnm);
      j++;
    }
  fclose(in_file);
  fprintf(out_file,";\n");
  strcpy(ln,compx);    /* now externals */
  strcat(ln,fn);
/*  strcat(ln,"."); */
  file_open(ln);
  fprintf(out_file,"\nextern void\n");
  filecopy();

/* COMPILED LISP FUNCTIONS */

  strcpy(ln,compc);
  strcat(ln,fn);
/*  strcat(ln,"."); */
  fprintf(out_file,"#include \"%s\"\n",ln);
  fclose(out_file);
  return;
}


/* main may have three optional arguments. arguments should be given in this
   order.
  arg 1) input file name base.
  arg 1) number of the first file. default first (1).
  arg 2) number of the last file. default all (100).  */

void main(argc,argv)
int argc;
char *argv[];
{ i1 = 1; i2 = 100;
  buf = (char *)malloc(16000);    /* set larger buffers for IO operations */
  obuf = (char *)malloc(20000);
  if(argc>=2) strcpy(argv1,argv[1]);
    else strcpy(argv1,"comp");
  strcpy(compn,argv1);
  strcat(compn,"n");
  strcpy(compx,argv1);
  strcat(compx,"x");
  strcpy(compc,argv1);
  strcat(compc,"c");
  if(argc>=3) sscanf(argv[2],"%d",&i1);
  if(argc>=4) sscanf(argv[3],"%d",&i2);

  for(file_no=i1; file_no<=i2; file_no++)
    { sprintf(fn,"%d",file_no);
      strcpy(ln,compn);
      strcat(ln,fn);
   /*   strcat(ln,"."); */
      in_file = fopen(ln, "r");
      if(in_file == NULL) break; else fclose(in_file);
      lispc(); }
  exit(0);
}                       /* end of main part */

