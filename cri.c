/* creator of the LISP-INI */
#include <stdio.h>
#include <string.h>

#define  outfile  "LISP-INI"
char comp[30],argv1[30],argv2[30],*buf,*obuf;
FILE *in_file, *out_file;

int i;


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
  if(buf != NULL) setvbuf(in_file,buf,_IOFBF,16000);
}

void main(argc,argv)
int argc;
char *argv[];
{
  buf = (char *)malloc(16000);
  obuf = (char *)malloc(20000);
  if( argc>=3) strcpy(argv2,argv[2]);
  else strcpy(argv2,outfile); 
  out_file = fopen(argv2,"w");
  if(obuf != NULL) setvbuf(out_file,obuf,_IOFBF,20000);

  if(argc>=2) strcpy(argv1,argv[1]);
    else strcpy(argv1,"comp");
  strcpy(comp,argv1);
  strcat(comp,"u");
  file_open(comp);
  filecopy();

  strcpy(comp,argv1);
  strcat(comp,"e");
  file_open(comp);
  filecopy();
  fprintf(out_file,"nil\n");
  fclose(out_file);
  exit(0);
}
