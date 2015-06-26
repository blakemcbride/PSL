/*  A LISP primitive tokenizer to be used with the STD-LISP interpreter
    written by E. Karabudak. Shall play the role of yylex of UNIX.

    * main function is yylex ... returns parsed expression bit in yytext
    * expects source from 'inputf' .. a filepointer
    * the pattern matcher tries to apply the test functions
          is....()
      these functions are responsible of disposing the characters which
      they have consumed by advancing the curpos pointer to the next
      virgin character. But if their test is unsuccessfull then it
      is also the responsebility of the function to back the pointer
      curpos to the point where that test function was entered, that
      means it  has to restore the virginity... funny ehh!?
*/

/* INPUTBUFLEN shall be minimum 200 */

#define INPUTBUFLEN 300

char input[INPUTBUFLEN];     /* input buffer */
/*char *curpos;         Has to be initialized to point to '0' in main */
char *lastpos;       /* Holds first position to start match on */
char signflag;       /* is 1/0 depending on whether sign is allowed or not */
char *ch1, *ch2;

/* The below is new in ver 7.0 */
/* corrects the problem of  rds() change => lose of unconsumed sexprs in  input buffer */ 

#define INPUTSAVETABLESIZE 30

typedef 
   struct INPUT_BUF_SAVE_ENTRY 
          { FILE *file; 
            char *curpos;
            char *lastpos;
            char input[INPUTBUFLEN]; } INPUT_BUF_SAVE_ENTRY ;

INPUT_BUF_SAVE_ENTRY input_buf_save_table[INPUTSAVETABLESIZE] ; 

/* end of ver 7.0 new part */
 



void exponent(), sign(), digits();
int soffinteger();

/*   PATTERN : 1  */

int isstring()
{  if (*curpos != '"') return(0);
  cont:
   curpos++;
   while (*curpos != '"') curpos++;
   curpos++;
   if (*curpos == '"') goto cont;
   return(1); }

/*   PATTERN : 2  */

int isexl()
{  if (*curpos != '!') return(0);
   curpos++;
   return(1); }

int isid()
{  if ((!isexl()) && (!isalpha(*curpos))) return(0);
   else curpos++;
   while(isalnum(*curpos) || isexl()) curpos++; /*patch 1*/
   return(1);  }

/*  PATTERN : 3  */

int isflo1()
{  char *start;

   start = curpos;
   digits();
   if(*curpos != '.') goto fail; else curpos++;
   if(! isdigit(*curpos)) goto fail;
   digits();
   exponent();
   return(1);
 fail:
   curpos = start;
   return(0);
}

int isflo2()
{  char *start;

   start = curpos;
   if(! isdigit(*curpos)) goto fail;
   digits();
   if(*curpos != '.') goto fail; else curpos++;
   digits();
   exponent();
   return(1);
 fail:
   curpos = start;
   return(0);
}

void exponent()
{  char *start;

   start = curpos;
  if( toupper(*curpos) != 'E') goto fail;  else curpos++;
   sign();
   if(! isdigit(*curpos)) goto fail;
   digits();
   return;
 fail:
   curpos=start;
}

int sonflo()
{  sign();
   return(isflo1() || isflo2());  }

/*  PATTERN : 4 & 6 */

int soninteger()
{  char *start;

   start = curpos;
   sign();
   if(soffinteger())   return(1);
   curpos=start;
   return(0);
}

int soffinteger()
{  if(!isdigit(*curpos)) return(0);        /*  no need to stack start pos */
   digitcnt = 0;
   while(isdigit(*curpos)){ digitcnt++; curpos++; }
   return(1);
}

/*  PATTERN : 5  */

int soffflo()
{  return(isflo1() || isflo2());  }

/*  PATTERN : 7  */

int isoctal(c)
{
  if( (c >= '0')  &&  (c <='7')) return(1);
  else return(0);  }

int isfpointer()
{  char *start;

   start = curpos;
   if(*curpos != '#')  goto fail;
   curpos++;
   if (!isoctal(*curpos)) goto fail;
   while(((curpos-start) <= 12) && isoctal(*curpos)) curpos++;
   if(*curpos != '#') goto fail;
   curpos++;
   return(1);
 fail:
   curpos=start;
   return(0);  }

/*  PATTERN : 8  */

int isnonspace()
{  if ((*curpos != ' ') && (*curpos != '\n') && (*curpos != '\t')) curpos++;
   else return(0);
   return(1);  }

/*  PATTERN : 9  */

int is_blank()
{  if ((*curpos == ' ') || (*curpos == '\t')) curpos++;
     else return(0);
   return(1);  }

/*  PATTERN : 10  */

int iseol()          /* comment part added. version 5.2   TY */
{  if (*curpos == '\n' || *curpos == '%') return(1);
   else return(0);  }

/*  UTILITIES  */

void sign()
{  char *start;
   start = curpos;
   if ((*curpos == '+') || (*curpos == '-')) curpos++;
   if (!isdigit(*curpos)) curpos = start;  }

void digits()
{  while(isdigit(*curpos)) curpos++;  }

void storetext()                 /*  copies matched text into yytext  */
{ int i;
  i = (int) (curpos-lastpos);
  strncpy(yytext,lastpos,i);
  yytext[i] = 0;
}

void unput(c)
char c;
{  *(--curpos) = c; }

void zycpsunput(c)
char *c;
{  ch1 = curpos;       /*  curpos plays the same role as yysptr does  */
   unput(' ');
   unput('"');
   unput(' ');
   ch2 = curpos;
   while (c != tx) unput(*(--c));
   return;  }

int zycpscheck()
{  if (curpos != ch2) {  curpos = ch1; return(1);  }
   curpos = ch1;
   return(0);  }


/*  SLAVE FUNCTIONS  */

void zysignonoff(x)
int x;
{  if (x) signflag = 1;
   else signflag = 0; }

int yylex()
{
  if (*curpos == 0)
   {
     start:
       if((curpos = fgets(input+100,INPUTBUFLEN-100,inputf)) == NULL)
         { yywrap(); return(EOF); };    /* ver [5.0]   .GU */
#ifndef TESTYYLEX
       if (echo.Xvalue!=NIL) zedit(curpos);
#endif
    }
  delimflag = 0;     /* That's the common case */
 match:
  lastpos = curpos;

  if (isstring()) { storetext(); return(Tstring); };
  if (isid())     { storetext(); return(Tid); };
  if (signflag)   { if (sonflo())     { storetext(); return(Tfloating); };
                    if (soninteger())
                       { storetext();
                         return(digitcnt <= DZ ? Tinteger : Tbig);} }
  else            { if (soffflo())    { storetext(); return(Tfloating); };
                    if (soffinteger())
                       { storetext();
                         return(digitcnt <= DZ ? Tinteger : Tbig);} };
  if (isfpointer())  { storetext(); return(Tfpointer); };
  if (iseol())    {
#ifndef TESTYYLEX
                    if (inputf == stdin && !null(value(&prompt)))
                     { if(idp(value(&prompt))) printf(pname(value(&prompt)));
                       else if(stringp(value(&prompt)))
                          printf(strbody(value(&prompt))); }
#endif
                    goto start; };
  if (isnonspace()) { delimflag = 1; storetext(); return(Tid); };
  if (is_blank())  goto match;
  zerror(8);
}

char yych()  /* This function is new, will be used by Readch ver [5.0] .GU */
{
  if(*curpos == 0)
   {if((curpos = fgets(input+100,INPUTBUFLEN-100,inputf)) == NULL)
      { yywrap(); return(EOF); };       /* ver [5.0]   .GU */
#ifndef TESTYYLEX
    if(echo.Xvalue!=NIL) zedit(curpos);
#endif
   }
  return(*(curpos++));
}


void yywrap()     /* was formerly in lisp-zfn.l ... don't know why? */
{
  if(inputf == stdin)
    { printf("end of std input file\n");
      exit(0); }
  else               /* new in 7.0..previously leak of this would cause core dump */
    { reg1 = NIL;    /* attention: No Close() is performed !!! */
      Rds(); }
}           
/* The below is new in ver 7.0 */

void store_input_buf_save_entry()  /* stores current input from inputf */
{ 
 int i;
 for (i=0; i<INPUTSAVETABLESIZE; i++) 
    if (input_buf_save_table[i].file==inputf) goto proceed;
 for (i=0; i<INPUTSAVETABLESIZE; i++) 
    if (!input_buf_save_table[i].file) goto proceed;
 /* if we are here no empty place in table ..=> issue error */
 zerror(42);
proceed:
    input_buf_save_table[i].file    =  inputf;
    input_buf_save_table[i].curpos  =  curpos;
    input_buf_save_table[i].lastpos = lastpos;
    memcpy(input_buf_save_table[i].input, input, INPUTBUFLEN);
}         


void restore_input_buf_save_entry()  /* restores current input of inputf  */
{                                    /* if ~exist in table=> no action    */
 int i;
 for (i=0; i<INPUTSAVETABLESIZE; i++) 
    if (input_buf_save_table[i].file==inputf)
      { 
        curpos  = input_buf_save_table[i].curpos;
        lastpos = input_buf_save_table[i].lastpos;
        memcpy(input, input_buf_save_table[i].input, INPUTBUFLEN);
        return;
      }
}         

       
void kill_input_buf_save_entry(FILE *f)  /* brutally removes current input from f */
{                                 /* no action if not exist */ 
 int i;
 for (i=0; i<INPUTSAVETABLESIZE; i++) 
    if (input_buf_save_table[i].file==f)
       { input_buf_save_table[i].file = NULL;
         break; }  /* only one existence is possible */
}           

