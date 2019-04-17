#include "symtab.h"

void initTable() {
  struct symbol* sp;
  for(sp=symTab; sp<&symTab[NSYM]; sp++) {
    sp->name = NULL;
    sp->value = 0;
    sp->type = -1;
    sp->row =-1;
  }
}

int hash(char* s) {
  int h = 5;
  int l = strlen(s);
  for(int i=0; i<l; i++) {
    h = h*37 + s[i];
  }
  return h;
}

struct symbol* searchA(char *s, int val, int typ, int r) {
  struct symbol* sp = &symTab[hash(s)%NSYM];
  int count = NSYM;
  while(--count > 0) {
    if(sp->name && !strcmp(sp->name, s))
      return sp;
    if(!sp->name) {
      sp->name = strdup(s);
      sp->value = val;
      sp->type= typ;
      sp->row=r;

      return sp;
    }

    if(++sp > symTab + NSYM) 
      sp = symTab;
  }
  printf("Symbol table full.\n");
  exit(1);
    
}

int declared(char *s) {
  int h = hash(s)%NSYM;
  struct symbol* sp = &symTab[h];
  if(!sp->name)
    return 0;
  while(sp->name) {
    if(!strcmp(sp->name, s))
      return 1;
    if(++sp > symTab + NSYM) sp = symTab;
  }
  return 0;
}

void printTable()
{
  int i;
  struct symbol* sp= &symTab[0];
  printf("<NAME\t, TYPE \t, ROW , INDEX> \n");
  for(i=0;i<NSYM;i++)
  {
    if(sp->name)
    {
      printf("%s %s %d %d\n",sp->name,"ID",sp->row,i);
    }
    sp++;
  }
}
