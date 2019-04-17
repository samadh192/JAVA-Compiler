#ifndef SYMTAB_H
#define SYMTAB_H

#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#define NSYM 1000

struct symbol {
  char* name;
  int value;
  int type;
  int row;

};

// Hash table using open addressing
struct symbol symTab[NSYM];

void initTable(void);

int hash(char* s);

struct symbol* searchA(char *s, int value, int type, int row);

int declared(char* s);

void printTable();


#endif
