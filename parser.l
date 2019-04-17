%{
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	#include "parser.tab.h"
	#include "symtab.h"
	//FUTURE JNULL OPERATOR OP_SHRR OP_DIM ASS_SHL ASS_SHR ASS_SHRR ASS_XOR BOOLLIT
	int line=1;
%}
BLANK [ ]
BLK [\b]
TAB [\t]
FF [\f]
CR [\r]
LF [\n]
Dimension [\[]({CR}|{LF}|{FF}|{TAB}|{BLK}|{BLANK}|{Comment})*[\]]
%%
\n {line++;}
Dimension { return OP_DIM;}
"," {printf("COMMA"); return COMMA;}
";" {printf("SC"); return SC;}
"." {printf("DOT"); return DOT;}
"=" {printf("EQT"); return EQT;}
"(" {printf("OB"); return OB;}
")" {printf("CB"); return CB;}
":" {printf("COLON"); return COLON;}
"[" {printf("OSB"); return OSB;}
"]" {printf("CSB"); return CSB;}
"~" {printf("TILDE"); return TILDE;}
"<" {printf("LT"); return LT;}
">" {printf("GT"); return GT;}
"^" {printf("POW"); return POW;}
"?" {printf("QMARK"); return QMARK;}
"{" {printf("OCB"); return OCB;}
"}" {printf("CCB"); return CCB;}
"abstract" {printf("ABSTRACT"); return ABSTRACT;}
"boolean" {printf("BOOLEAN"); return BOOLEAN;}
"break" {printf("BREAK"); return BREAK;}
"byte" {printf("BYTE"); return BYTE;}
"case" {printf("CASE"); return CASE;}
"cast" {printf("CAST"); return CAST;}
"catch" {printf("CATCH"); return CATCH;}
"char" {printf("CHAR"); return CHAR;}
"class" {printf("CLASS"); return CLASS;}
"const" {printf("CONST"); return CONST;}
"continue" {printf("CONTINUE"); return CONTINUE;}
"default" {printf("DEFAULT"); return DEFAULT;}
"do" {printf("DO"); return DO;}
"double" {printf("DOUBLE"); return DOUBLE;}
"else" {printf("ELSE"); return ELSE;}
"extends" {printf("EXTENDS"); return EXTENDS;}
"final" {printf("FINAL"); return FINAL;}
"finally" {printf("FINALLY"); return FINALLY;}
"float" {printf("FLOAT"); return FLOAT;}
"for" {printf("FOR"); return FOR;}
"generic" {printf("GENERIC"); return GENERIC;}
"goto" {printf("GOTO"); return GOTO;}
"if" {printf("IF"); return IF;}
"implements" {printf("IMPLEMENTS"); return IMPLEMENTS;}
"import" {printf("IMPORT"); return IMPORT;}
"inner" {printf("INNER"); return INNER;}
"instanceof" {printf("INSTANCEOF"); return INSTANCEOF;}
"int" {printf("INT"); return INT;}
"interface" {printf("INTERFACE"); return INTERFACE;}
"long" {printf("LONG"); return LONG;}
"native" {printf("NATIVE"); return NATIVE;}
"new" {printf("NEW"); return NEW;}
"outer" {printf("OUTER"); return OUTER;}
"package" {printf("PACKAGE"); return PACKAGE;}
"private" {printf("PRIVATE"); return PRIVATE;}
"protected" {printf("PROTECTED"); return PROTECTED;}
"public" {printf("PUBLIC"); return PUBLIC;}
"rest" {printf("REST"); return REST;}
"return" {printf("RETURN"); return RETURN;}
"short" {printf("SHORT"); return SHORT;}
"static" {printf("STATIC"); return STATIC;}
"super" {printf("SUPER"); return SUPER;}
"switch" {printf("SWITCH"); return SWITCH;}
"synchronized" {printf("SYNCHRONIZED"); return SYNCHRONIZED;}
"this" {printf("THIS"); return THIS;}
"throw" {printf("THROW"); return THROW;}
"throws" {printf("THROWS"); return THROWS;}
"transient" {printf("TRANSIENT"); return TRANSIENT;}
"try" {printf("TRY"); return TRY;}
"var" {printf("VAR"); return VAR;}
"void" {printf("VOID"); return VOID;}
"volatile" {printf("VOLATILE"); return VOLATILE;}
"while" {printf("WHILE"); return WHILE;}
"++" {printf("OP_INC"); return OP_INC;}
"--" {printf("OP_DEC"); return OP_DEC;}
"<<" {printf("OP_SHL"); return OP_SHL;}
">>" {printf("OP_SHR"); return OP_SHR;}
">=" {printf("OP_GE"); return OP_GE;}
"<=" {printf("OP_LE"); return OP_LE;}
"==" {printf("OP_EQ"); return OP_EQ;}
"!=" {printf("OP_NE"); return OP_NE;}
"!" {printf("NOT"); return NOT;}
"&&" {printf("OP_LAND"); return OP_LAND;}
"||" {printf("OP_LOR"); return OP_LOR;}
"*" {printf("ASS_MUL"); return ASS_MUL;}
"/" {printf("ASS_DIV"); return ASS_DIV;}
"%" {printf("ASS_MOD"); return ASS_MOD;}
"+" {printf("ASS_ADD"); return ASS_ADD;}
"-" {printf("ASS_SUB"); return ASS_SUB;}
"&" {printf("ASS_AND"); return ASS_AND;}
"|" {printf("ASS_OR"); return ASS_OR;}
[a-zA-Z_]+ {printf("%s %d",yytext,IDENTIFIER);
	searchA(yytext,0,IDENTIFIER,yylineno); return IDENTIFIER;}
[0-9]+ {printf("%s",yytext); return NUMBER;}
\"([^\\\"]|\\.)*\" 	{printf("%s",yytext); return LITERAL;}
%%
int yywrap()
{
	return 1;
}
