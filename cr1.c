#include <stdio.h>
#include <string.h>

#define  outfile  "lisp1.c"

#include "flags.l"
#include "crfile.h"


char hsh[128][50];
char nm[32], pnm[32], tp[20], val[32],compn1[30],compn[30],argv1[30];
char ln[132],*buf,*obuf;
int i, func_cnt, k, no, hs;
#if BITF
int x1,x2,x3,x4,x5,x6;
#else
int x1;
#endif
FILE *in_file, *out_file;

int hash(xx)
char *xx;
{
  int h;
  h = 0;
  while(*xx) h += *(xx++);
  return(h%128);
}

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

void main(argc,argv)
int argc;
char *argv[];
{
  if(argc>=2) strcpy(argv1,argv[1]);
    else strcpy(argv1,"comp");
  strcpy(compn,argv1);
  strcat(compn,"n");
  strcpy(compn1,argv1);
  strcat(compn1,"n1");
  buf = (char *)malloc(16000);
  obuf = (char *)malloc(20000);
  out_file = fopen(outfile,"w");
  if(obuf != NULL) setvbuf(out_file,obuf,_IOFBF,20000);
#if _TURBOC_
  fprintf(out_file,"\nextern unsigned _stklen = 10000;\n");  /*used by turboc */
#endif
/*  FLAGS  */
  file_open(flags);
  filecopy();

/*  TYPES AND MACROS  */
  file_open(types);
  filecopy();
/*  file_open(type);
  filecopy(); */

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
  strcpy(nm,argv1);
  strcat(nm,"u");
  in_file = fopen(nm,"r");
  if(in_file != NULL)
    { setvbuf(in_file,buf,_IOFBF,16000);
      fscanf(in_file, "%d", &no);
      fclose(in_file);
      fprintf(out_file,"PSEXP urwelt\[%d\];\n", no);
    }
  else
    { no = 0;
      fprintf(out_file,"PSEXP urwelt\[1\];\n"); }
  fprintf(out_file,"unsigned ursize = %d;\n", no);

/*  FUNCTION COUNTING  */

  file_open(fnames);
  func_cnt = 0;
  fprintf(out_file,"\nvoid ");
  while(fscanf(in_file,  "%s%s%d%s", nm, pnm, &no, tp) != EOF)
    { if(func_cnt != 0) fprintf(out_file,",");
      if(func_cnt % 6 == 0) fprintf(out_file,"\n    ");
      strcat(pnm, "()");
      fprintf(out_file," %s", pnm);
      func_cnt++;
    }
  fclose(in_file);
  i = 2;
  in_file = fopen(compn1, "r");
  while(in_file != NULL)
     {setvbuf(in_file,buf,_IOFBF,16000);
      while(fscanf(in_file,  "%s%s%d%s", nm, pnm, &no, tp) != EOF)
	{ if(func_cnt != 0) fprintf(out_file,",");
	  if(func_cnt % 6 == 0) fprintf(out_file,"\n    ");
	  strcat(pnm, "()");
	  fprintf(out_file," %s", pnm);
	  func_cnt++;
	}
      fclose(in_file);
      sprintf(tp,"%d",i);
      i++;
      strcpy(ln,compn);
      strcat(ln,tp);
      /*  strcat(ln,"."); */
      in_file = fopen(ln, "r");
    }
  fprintf(out_file,";\n");

  for(i=0; i<128; i++) strcpy(hsh[i], "NULL");

/* ASCII CONTROL CHARACTER STRINGS */

  fprintf(out_file,"\nchar asciich\[32\]\[2\] = {");
  for(i=0; i<' '; i++)
    fprintf(out_file,"\n    { '\\%o', '\\0' },", i);
  fprintf(out_file," };\n");
  fprintf(out_file,"char ascii127\[2\] = { '\\127', '\\0' };\n");

/*  SYSTEM IDENTIFIERS  */

  file_open(sysids);
  fprintf(out_file,"\n");
#if BITF
  while(fscanf(in_file,"%s%d%d%d%d%d%d%s%s",
			     nm,&x1,&x2,&x3,&x4,&x5,&x6,pnm,val) != EOF)
    { hs = hash(pnm);
      fprintf(out_file,
	       "ID %s = {Tid, %d,%d,%d,%d,%d,%d, %s, %s, NIL, \"%s\" };\n",
	       nm, x1,x2,x3,x4,x5,x6, hsh[hs], val, pnm);
      sprintf(hsh[hs], "&%s", nm);
    }
#else
  while(fscanf(in_file, "%s%d%s%s",nm, &x1, pnm, val) != EOF)
    { hs = hash(pnm);
      fprintf(out_file,"ID %s = {Tid, %d, %s, %s, NIL, \"%s\" };\n",
                       nm, x1, hsh[hs], val, pnm);
      sprintf(hsh[hs], "&%s", nm);
    }
#endif
  fclose(in_file);

/* CHARACTER IDENTIFIERS */

  fprintf(out_file,"\nID chrid[128] = {\n");
  for(i=0; i<127; i++)
    { if(i<' ')
#if BITF
	fprintf(out_file,
	    "    {Tid, 0,0,0,0,1,0, %s, NULL, NIL, asciich\[%d\] },\n"
	    , hsh[i], i);
      else if(i == 't')
	fprintf(out_file,
	    "    {Tid, 0,1,0,0,1,0, %s, Sexp(&chrid\['t'\]), NIL, \"%c\" },\n"
	    , hsh[i], i);
      else if(i == '"' || i == '\\')
        fprintf(out_file,"    {Tid, 0,0,0,0,1,0, %s, NULL, NIL, \"\\%c\" },\n"
                    , hsh[i], i);
      else
        fprintf(out_file,"    {Tid, 0,0,0,0,1,0, %s, NULL, NIL, \"%c\" },\n"
                    , hsh[i], i);
      sprintf(hsh[i], "&chrid\[%d\]", i);
    }
  fprintf(out_file,"    {Tid, 0,0,0,0,1,0, %s, NULL, NIL, ascii127 } };\n"
		    , hsh[127]);
#else
        fprintf(out_file,"    {Tid, 2, %s, NULL, NIL, asciich\[%d\] },\n"
                    , hsh[i], i);
      else if(i == 't')
        fprintf(out_file,"    {Tid, 18, %s, Sexp(&chrid\['t'\]), NIL, \"%c\" },\n"
                    , hsh[i], i);
      else if(i == '"' || i == '\\')
        fprintf(out_file,"    {Tid, 2, %s, NULL, NIL, \"\\%c\" },\n"
                    , hsh[i], i);
      else
        fprintf(out_file,"    {Tid, 2, %s, NULL, NIL, \"%c\" },\n"
                    , hsh[i], i);
      sprintf(hsh[i], "&chrid\[%d\]", i);
    }
  fprintf(out_file,"    {Tid, 2, %s, NULL, NIL, ascii127 } };\n"
		    , hsh[127]);
#endif
  strcpy(hsh[127],"&chrid[127]");

/* FUNCTION POINTERS */

  file_open(fnames);
  fprintf(out_file,"\nFPOINTER fnpntr\[%d\] = {",func_cnt);
  while(fscanf(in_file, "%s%s%d%s", nm, pnm, &no, tp) != EOF)
    { fprintf(out_file,"\n    { Tfpointer, %d, %s },", no, pnm); }
  fclose(in_file);
  in_file = fopen(compn1, "r");
  k = 2;
  while(in_file != NULL)
    { setvbuf(in_file,buf,_IOFBF,16000);
      while(fscanf(in_file,  "%s%s%d%s", nm, pnm, &no, tp) != EOF)
        { fprintf(out_file,"\n    { Tfpointer, %d, %s },", no, pnm); }
      fclose(in_file);
      sprintf(tp,"%d",k);
      k++;
      strcpy(ln,compn);
      strcat(ln,tp);
     /*  strcat(ln,"."); */
      in_file = fopen(ln, "r");
    }
  fprintf(out_file," };\n");

/* FUNCTION VALUE PAIRS */

  fprintf(out_file,"\nint NOFPAIR = %d;\n",func_cnt);

  file_open(fnames);
  i = 0;
  fprintf(out_file,"\nPAIR fnvlpr\[%d\] = {",func_cnt);
  while(fscanf(in_file, "%s%s%d%s", nm, pnm, &no, tp) != EOF)
    { fprintf(out_file,"\n    { Tpair, Sexp(&%s), Sexp(&fnpntr\[%d\]) },", tp, i);
      i++; }
  fclose(in_file);
  in_file = fopen(compn1, "r");
  k = 2;
  while(in_file != NULL)
    { setvbuf(in_file,buf,_IOFBF,16000);
      while(fscanf(in_file,  "%s%s%d%s", nm, pnm, &no, tp) != EOF)
        { fprintf(out_file,"\n    { Tpair, Sexp(&%s), Sexp(&fnpntr\[%d\]) },", tp, i);
          i++; }
      fclose(in_file);
      sprintf(tp,"%d",k);
      k++;
      strcpy(ln,compn);
      strcat(ln,tp);
     /*  strcat(ln,"."); */
      in_file = fopen(ln, "r");
    }
  fprintf(out_file," };\n");

/* FUNCTION NAME IDENTIFIERS */

  file_open(fnames);
  i = 0;
  fprintf(out_file,"\nID fname\[%d\] = {", func_cnt);
  while(fscanf(in_file, "%s%s%d%s", nm, pnm, &no, tp) != EOF)
    { hs = hash(nm);
#if BITF
      fprintf(out_file,
	    "    {Tid, 0,0,0,1,1,0, %s, Sexp(&fnvlpr\[%d\]), NIL, \"%s\" },\n"
	    , hsh[hs], i, nm);
#else
      fprintf(out_file,"    {Tid, 6, %s, Sexp(&fnvlpr\[%d\]), NIL, \"%s\" },\n"
                    , hsh[hs], i, nm);
#endif
      sprintf(hsh[hs], "&fname\[%d\]", i);
      i++;
    }
  fclose(in_file);
  k = 2;
  in_file = fopen(compn1, "r");
  while(in_file != NULL)
    { setvbuf(in_file,buf,_IOFBF,16000);
      while(fscanf(in_file,  "%s%s%d%s", nm, pnm, &no, tp) != EOF)
        { hs = hash(nm);
#if BITF
	  fprintf(out_file,
	    "    {Tid, 0,0,0,1,1,0, %s, Sexp(&fnvlpr\[%d\]), NIL, \"%s\" },\n"
	    , hsh[hs], i, nm);
#else
	  fprintf(out_file,
	    "    {Tid, 6, %s, Sexp(&fnvlpr\[%d\]), NIL, \"%s\" },\n"
	    , hsh[hs], i, nm);
#endif
          sprintf(hsh[hs], "&fname\[%d\]", i);
          i++;
         }
      fclose(in_file);
      sprintf(tp,"%d",k);
      k++;
      strcpy(ln,compn);
      strcat(ln,tp);
      /* strcat(ln,"."); */
      in_file = fopen(ln, "r");
    }
  fprintf(out_file," };\n");


/* HASH TABLE */

  fprintf(out_file,"\nPID hashtab[128] = {");
  for(i=0; i<128; i++)
    { if(i%4 == 0) fprintf(out_file,"\n");
      strcat(hsh[i], ",");
      fprintf(out_file," %s", hsh[i]); }
  fprintf(out_file," };\n");
  fclose(out_file);
  exit(0);
}                             /*end of main */
