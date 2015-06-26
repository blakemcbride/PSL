#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "flags.l"
#include "crfile.h"

#define  outfile  "lisp2.c"


char nm[32], pnm[32], tp[30], val[32];
char ln[132],*buf,*obuf;
int i, j, k, no, hs;
FILE *in_file, *out_file;
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

void filecopy1()
{
  int c;
  int n;

  n = 0;
  while((c = getc(in_file)) != EOF)
    if( c == '@')
      fprintf(out_file, "Sexp(&fname\[%d\])", n);
    else if( c == '$')
      fprintf(out_file, "%d", n);
    else if( c == '/')
      { putc(c, out_file);
        c = getc(in_file);
        if( c == EOF) break;
        putc(c, out_file);
        if( c == '*')
          { c = getc(in_file);
            if( c == EOF) break;
            putc(c, out_file);
            if( c == '@')
              { n++;
                fprintf(out_file,"*/\nvoid ");
                getc(in_file);getc(in_file);getc(in_file); }
          }
      }
    else
      putc(c, out_file);
  fprintf(out_file,"\n");
  fclose(in_file);
}

void main()
{
  buf = (char *)malloc(16000);    /* set larger buffers for IO operations */
  obuf = (char *)malloc(20000);
  out_file = fopen(outfile,"w");
  if(obuf != NULL) setvbuf(out_file,obuf,_IOFBF,20000);

/*  FLAGS  */
  file_open(flags);
  filecopy();

/*  TYPES AND MACROS  */
  file_open(types);
  filecopy();
  file_open(type);
  filecopy();
/*  QUOTE  */

  file_open(fnames);
  i = 0;
  while(fscanf(in_file,  "%s%s%d%s", nm, pnm, &no, tp) != EOF)
    { if(strcmp(nm, "quote") == 0)
        { fprintf(out_file,"#define quote (Sexp(&fname\[%d\]))\n", i);
          break;
        }
      i++;
    }
  fclose(in_file);

/* URWELT ARRAY */

  fprintf(out_file,"extern PSEXP urwelt\[\];\n");
  fprintf(out_file,"extern unsigned ursize;\n");
  fprintf(out_file,"extern PPAIR fnvlpr\[\];\n");

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

/* HASH TABLE */

  fprintf(out_file,"\nextern PID hashtab[128];\n");

/*  ERROR MESSAGES   */

  file_open(errors);
  fprintf(out_file,"\nchar *emessages\[\] = {");
  while(fgets(ln, 80, in_file) != NULL)
    { ln[strlen(ln)-1] = 0;
      fprintf(out_file,"\n  \"%s\",", ln);
    }
  fprintf(out_file," };\n");
  fclose(in_file);

/* HEADER DECLARATIONS */

  file_open(header);
  filecopy();

/*  FUNCTION DECLARATIONS  */

  file_open(fnames);
  fprintf(out_file,"\nextern void");
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

/* AUXILIARY FUNCTION declarations */

  file_open(zfnames);
  fprintf(out_file,"\n");
  while(fscanf(in_file, "%s%s", tp, nm) != EOF)
    fprintf(out_file,"%s %s();\n", tp, nm);
  fclose(in_file);

/* TOKENIZER */

  file_open(yylex);
  filecopy();

/* BIG NUMBERS */

  file_open(big);
  filecopy();

/* AUXILIARY FUNCTIONS */

  file_open(lispzfn);
  filecopy();

/* LISP FUNCTIONS */

  file_open(lispfn);
  filecopy1();


  fclose(out_file);
  exit(0);
}                                    /* end of main */

