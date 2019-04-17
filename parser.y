%{
#include<stdio.h>
#include<stdlib.h>
#include "symtab.h"

int yylex();
int yyerror();
extern FILE *yyin;
extern int yylineno;
%}

%token ABSTRACT
%token BOOLEAN BREAK BYTE BYVALUE
%token CASE CAST CATCH CHAR CLASS CONST CONTINUE CCB COMMA
%token DEFAULT DO DOUBLE SC DOT EQT OB CB COLON OSB CSB TILDE NOT
%token ELSE EXTENDS LT GT POW QMARK
%token FINAL FINALLY FLOAT FOR FUTURE
%token GENERIC GOTO
%token IF IMPLEMENTS IMPORT INNER INSTANCEOF INT INTERFACE
%token LONG
%token NATIVE NEW JNULL
%token OPERATOR OUTER
%token PACKAGE PRIVATE PROTECTED PUBLIC
%token REST RETURN
%token SHORT STATIC SUPER SWITCH SYNCHRONIZED
%token THIS THROW THROWS TRANSIENT TRY
%token VAR VOID VOLATILE
%token WHILE
%token OP_INC OP_DEC
%token OP_SHL OP_SHR OP_SHRR
%token OP_GE OP_LE OP_EQ OP_NE
%token OP_LAND OP_LOR
%token OP_DIM OCB 
%token ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB
%token ASS_SHL ASS_SHR ASS_SHRR ASS_AND ASS_XOR ASS_OR
%token IDENTIFIER LITERAL BOOLLIT NUMBER

%start CompilationUnit
%define parse.error verbose
%%

TypeSpecifier
	: TypeName
	| TypeName Dims
	;

TypeName
	: PrimitiveType
	| QualifiedName
	;

ClassNameList
        : QualifiedName
        | ClassNameList COMMA QualifiedName
	;

PrimitiveType
	: BOOLEAN
	| CHAR
	| BYTE
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| VOID
	;

SemiColons
	: SC
        | SemiColons SC
        ;

CompilationUnit
	: ProgramFile
        ;

ProgramFile
	: PackageStatement ImportStatements TypeDeclarations
	| PackageStatement ImportStatements
	| PackageStatement                  TypeDeclarations
	|                  ImportStatements TypeDeclarations
	| PackageStatement
	|                  ImportStatements
	|                                   TypeDeclarations
	;

PackageStatement
	: PACKAGE QualifiedName SemiColons
	;

TypeDeclarations
	: TypeDeclarationOptSemi
	| TypeDeclarations TypeDeclarationOptSemi
	;

TypeDeclarationOptSemi
        : TypeDeclaration
        | TypeDeclaration SemiColons
        ;

ImportStatements
	: ImportStatement
	| ImportStatements ImportStatement
	;

ImportStatement
	: IMPORT QualifiedName SemiColons
	| IMPORT QualifiedName DOT ASS_MUL SemiColons
	;

QualifiedName
	: IDENTIFIER
	| QualifiedName DOT IDENTIFIER
	;

TypeDeclaration
	: ClassHeader OCB FieldDeclarations CCB
	| ClassHeader OCB CCB
	;

ClassHeader
	: Modifiers ClassWord IDENTIFIER Extends Interfaces
	| Modifiers ClassWord IDENTIFIER Extends
	| Modifiers ClassWord IDENTIFIER       Interfaces
	|           ClassWord IDENTIFIER Extends Interfaces
	| Modifiers ClassWord IDENTIFIER
	|           ClassWord IDENTIFIER Extends
	|           ClassWord IDENTIFIER       Interfaces
	|           ClassWord IDENTIFIER
	;

Modifiers
	: Modifier
	| Modifiers Modifier
	;

Modifier
	: ABSTRACT
	| FINAL
	| PUBLIC
	| PROTECTED
	| PRIVATE
	| STATIC
	| TRANSIENT
	| VOLATILE
	| NATIVE
	| SYNCHRONIZED
	;

ClassWord
	: CLASS
	| INTERFACE
	;

Interfaces
	: IMPLEMENTS ClassNameList
	;

FieldDeclarations
	: FieldDeclarationOptSemi
        | FieldDeclarations FieldDeclarationOptSemi
	;

FieldDeclarationOptSemi
        : FieldDeclaration
        | FieldDeclaration SemiColons
        ;

FieldDeclaration
	: FieldVariableDeclaration SC
	| MethodDeclaration
	| ConstructorDeclaration
	| StaticInitializer
        | NonStaticInitializer
        | TypeDeclaration
	;

FieldVariableDeclaration
	: Modifiers TypeSpecifier VariableDeclarators
	|           TypeSpecifier VariableDeclarators
	;

VariableDeclarators
	: VariableDeclarator
	| VariableDeclarators COMMA VariableDeclarator
	;

VariableDeclarator
	: DeclaratorName
	| DeclaratorName OSB CSB
	| DeclaratorName OSB CSB OSB CSB
	| DeclaratorName OSB CSB EQT OCB dec_list CCB
	| DeclaratorName EQT VariableInitializer
	;
dec_list
	: NUMBER COMMA dec_list
	| NUMBER
	| LITERAL COMMA dec_list
	| LITERAL
	|
	;

VariableInitializer
	: Expression
	| OCB CCB
        | OCB ArrayInitializers CCB
        ;

ArrayInitializers
	: VariableInitializer
	| ArrayInitializers COMMA VariableInitializer
	| ArrayInitializers COMMA
	;

MethodDeclaration
	: Modifiers TypeSpecifier MethodDeclarator Throws MethodBody
	| Modifiers TypeSpecifier MethodDeclarator        MethodBody
	|           TypeSpecifier MethodDeclarator Throws MethodBody
	|           TypeSpecifier MethodDeclarator        MethodBody
	;

MethodDeclarator
	: DeclaratorName OB ParameterList CB
	| DeclaratorName OB CB
	| MethodDeclarator OP_DIM
	;

ParameterList
	: Parameter
	| ParameterList COMMA Parameter
	;

Parameter
	: TypeSpecifier DeclaratorName
	| TypeSpecifier DeclaratorName OSB CSB
	| TypeSpecifier DeclaratorName OSB CSB OSB NUMBER CSB
	| TypeSpecifier OSB CSB DeclaratorName 
	| TypeSpecifier OSB CSB OSB NUMBER CSB DeclaratorName 
        | FINAL TypeSpecifier DeclaratorName
        | DeclaratorName
	;

DeclaratorName
	: IDENTIFIER
        | DeclaratorName OP_DIM
        ;

Throws
	: THROWS ClassNameList
	;

MethodBody
	: Block
	| SC
	;

ConstructorDeclaration
	: Modifiers ConstructorDeclarator Throws Block
	| Modifiers ConstructorDeclarator        Block
	|           ConstructorDeclarator Throws Block
	|           ConstructorDeclarator        Block
	;

ConstructorDeclarator
	: IDENTIFIER OB ParameterList CB
	| IDENTIFIER OB CB
	;

StaticInitializer
	: STATIC Block
	;

NonStaticInitializer
        : Block
        ;

Extends
	: EXTENDS TypeName
	| Extends COMMA TypeName
	;

Block
	: OCB LocalVariableDeclarationsAndStatements CCB
	| OCB CCB
        ;

LocalVariableDeclarationsAndStatements
	: LocalVariableDeclarationOrStatement
	| LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
	;

LocalVariableDeclarationOrStatement
	: LocalVariableDeclarationStatement
	| Statement
	;

LocalVariableDeclarationStatement
	: TypeSpecifier VariableDeclarators SC
        | FINAL TypeSpecifier VariableDeclarators SC
	;

Statement
	: EmptyStatement
	| LabelStatement
	| ExpressionStatement SC
        | SelectionStatement
        | IterationStatement
	| JumpStatement
	| GuardingStatement
	| Block
	;

EmptyStatement
	: SC
        ;

LabelStatement
	: IDENTIFIER COLON
	| CASE NUMBER COLON
    | CASE ConstantExpression COLON
	| DEFAULT COLON
        ;

ExpressionStatement
	: Expression
	;

SelectionStatement
	: IF OB Expression CB Statement
        | IF OB Expression CB Statement ELSE Statement
        | SWITCH OB Expression CB Block
        ;

IterationStatement
	: WHILE OB Expression CB Statement
	| DO Statement WHILE OB Expression CB SC
	| FOR OB ForInit ForExpr ForIncr CB Statement
	| FOR OB ForInit ForExpr         CB Statement
	;

ForInit
	: ExpressionStatements SC
	| LocalVariableDeclarationStatement
	| SC
	;

ForExpr
	: Expression SC
	| SC
	;

ForIncr
	: ExpressionStatements
	;

ExpressionStatements
	: ExpressionStatement
	| ExpressionStatements COMMA ExpressionStatement
	;

JumpStatement
	: BREAK IDENTIFIER SC
	| BREAK            SC
    | CONTINUE IDENTIFIER SC
	| CONTINUE            SC
	| RETURN Expression SC
	| RETURN            SC
	| THROW Expression SC
	;

GuardingStatement
	: SYNCHRONIZED OB Expression CB Statement
	| TRY Block Finally
	| TRY Block Catches
	| TRY Block Catches Finally
	;

Catches
	: Catch
	| Catches Catch
	;

Catch
	: CatchHeader Block
	;

CatchHeader
	: CATCH OB TypeSpecifier IDENTIFIER CB
	| CATCH OB TypeSpecifier CB
	;

Finally
	: FINALLY Block
	;

PrimaryExpression
	: QualifiedName
	| NotJustName
	;

NotJustName
	: SpecialName
	| NewAllocationExpression
	| ComplexPrimary
	;

ComplexPrimary
	: OB Expression CB
	| ComplexPrimaryNoParenthesis
	;

ComplexPrimaryNoParenthesis
	: LITERAL
	| BOOLLIT
	| ArrayAccess
	| FieldAccess
	| MethodCall
	;

ArrayAccess
	: QualifiedName OSB Expression CSB
	| ComplexPrimary OSB Expression CSB
	;

FieldAccess
	: NotJustName DOT IDENTIFIER
	| RealPostfixExpression DOT IDENTIFIER
        | QualifiedName DOT THIS
        | QualifiedName DOT CLASS
        | PrimitiveType DOT CLASS
	;

MethodCall
	: MethodAccess OB ArgumentList CB
	| MethodAccess OB CB
	;

MethodAccess
	: ComplexPrimaryNoParenthesis
	| SpecialName
	| QualifiedName
	;

SpecialName
	: THIS
	| SUPER
	| JNULL
	;

ArgumentList
	: Expression
	| ArgumentList COMMA Expression
	;

NewAllocationExpression
        : PlainNewAllocationExpression
        | QualifiedName DOT PlainNewAllocationExpression
        ;

PlainNewAllocationExpression
    	: ArrayAllocationExpression
    	| ClassAllocationExpression
    	| ArrayAllocationExpression OCB CCB
    	| ClassAllocationExpression OCB CCB
    	| ArrayAllocationExpression OCB ArrayInitializers CCB
    	| ClassAllocationExpression OCB FieldDeclarations CCB
    	;

ClassAllocationExpression
	: NEW TypeName OB ArgumentList CB
	| NEW TypeName OB              CB
        ;

ArrayAllocationExpression
	: NEW TypeName DimExprs Dims
	| NEW TypeName DimExprs
        | NEW TypeName Dims
	;

DimExprs
	: DimExpr
	| DimExprs DimExpr
	;

DimExpr
	: OSB Expression CSB
	;

Dims
	: OP_DIM
	| Dims OP_DIM
	;

PostfixExpression
	: PrimaryExpression
	| RealPostfixExpression
	;

RealPostfixExpression
	: PostfixExpression OP_INC
	| PostfixExpression OP_DEC
	;

UnaryExpression
	: OP_INC UnaryExpression
	| OP_DEC UnaryExpression
	| ArithmeticUnaryOperator CastExpression
	| LogicalUnaryExpression
	;

LogicalUnaryExpression
	: PostfixExpression
	| LogicalUnaryOperator UnaryExpression
	;

LogicalUnaryOperator
	: TILDE
	| NOT
	;

ArithmeticUnaryOperator
	: ASS_ADD
	| ASS_SUB
	;

CastExpression
	: UnaryExpression
	| OB PrimitiveTypeExpression CB CastExpression
	| OB ClassTypeExpression CB CastExpression
	| OB Expression CB LogicalUnaryExpression
	;

PrimitiveTypeExpression
	: PrimitiveType
        | PrimitiveType Dims
        ;

ClassTypeExpression
	: QualifiedName Dims
        ;

MultiplicativeExpression
	: CastExpression
	| MultiplicativeExpression ASS_MUL CastExpression
	| MultiplicativeExpression ASS_DIV CastExpression
	| MultiplicativeExpression ASS_MOD CastExpression
	;

AdditiveExpression
	: MultiplicativeExpression
        | AdditiveExpression ASS_ADD MultiplicativeExpression
	| AdditiveExpression ASS_SUB MultiplicativeExpression
        ;

ShiftExpression
	: AdditiveExpression
        | ShiftExpression OP_SHL AdditiveExpression
        | ShiftExpression OP_SHR AdditiveExpression
        | ShiftExpression OP_SHRR AdditiveExpression
	;

RelationalExpression
	: ShiftExpression
    | RelationalExpression LT ShiftExpression
	| RelationalExpression GT ShiftExpression
	| RelationalExpression OP_LE ShiftExpression
	| RelationalExpression OP_GE ShiftExpression
	| RelationalExpression INSTANCEOF TypeSpecifier
	;

EqualityExpression
	: RelationalExpression
        | EqualityExpression OP_EQ RelationalExpression
        | EqualityExpression OP_NE RelationalExpression
        ;

AndExpression
	: EqualityExpression
        | AndExpression ASS_AND EqualityExpression
        ;

ExclusiveOrExpression
	: AndExpression
	| ExclusiveOrExpression POW AndExpression
	;

InclusiveOrExpression
	: ExclusiveOrExpression
	| InclusiveOrExpression ASS_OR ExclusiveOrExpression
	;

ConditionalAndExpression
	: InclusiveOrExpression
	| ConditionalAndExpression OP_LAND InclusiveOrExpression
	;

ConditionalOrExpression
	: ConditionalAndExpression
	| ConditionalOrExpression OP_LOR ConditionalAndExpression
	;

ConditionalExpression
	: ConditionalOrExpression
	| ConditionalOrExpression QMARK Expression COLON ConditionalExpression
	;

AssignmentExpression
	: ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression
	| NUMBER
	;

AssignmentOperator
	: EQT
	| ASS_MUL
	| ASS_DIV
	| ASS_MOD
	| ASS_ADD
	| ASS_SUB
	| ASS_SHL
	| ASS_SHR
	| ASS_SHRR
	| ASS_AND
	| ASS_XOR
	| ASS_OR
	;

Expression
	: AssignmentExpression 
	

	| MethodDeclarator LT IDENTIFIER
	| MethodDeclarator OP_LE IDENTIFIER
	| MethodDeclarator GT IDENTIFIER
	| MethodDeclarator OP_GE IDENTIFIER
	| MethodDeclarator OP_NE IDENTIFIER
	| MethodDeclarator OP_EQ IDENTIFIER

	| MethodDeclarator LT MethodDeclarator
	| MethodDeclarator OP_LE MethodDeclarator
	| MethodDeclarator GT MethodDeclarator
	| MethodDeclarator OP_GE MethodDeclarator
	| MethodDeclarator OP_NE MethodDeclarator
	| MethodDeclarator OP_EQ MethodDeclarator

	| NUMBER LT MethodDeclarator
	| NUMBER OP_LE MethodDeclarator
	| NUMBER GT MethodDeclarator
	| NUMBER OP_GE MethodDeclarator
	| NUMBER OP_NE MethodDeclarator
	| NUMBER OP_EQ MethodDeclarator

	| MethodDeclarator LT NUMBER
	| MethodDeclarator OP_LE NUMBER
	| MethodDeclarator GT NUMBER
	| MethodDeclarator OP_GE NUMBER
	| MethodDeclarator OP_NE NUMBER
	| MethodDeclarator OP_EQ NUMBER

	| IDENTIFIER LT NUMBER
	| IDENTIFIER OP_LE NUMBER
	| IDENTIFIER GT NUMBER
	| IDENTIFIER OP_GE NUMBER
	| IDENTIFIER OP_NE NUMBER
	| IDENTIFIER OP_EQ NUMBER

	| IDENTIFIER LT IDENTIFIER
	| IDENTIFIER OP_LE IDENTIFIER
	| IDENTIFIER GT IDENTIFIER
	| IDENTIFIER OP_GE IDENTIFIER
	| IDENTIFIER OP_NE IDENTIFIER
	| IDENTIFIER OP_EQ IDENTIFIER

	| NUMBER LT NUMBER
	| NUMBER OP_LE NUMBER
	| NUMBER GT NUMBER
	| NUMBER OP_GE NUMBER
	| NUMBER OP_NE NUMBER
	| NUMBER OP_EQ NUMBER
        ;

ConstantExpression
	: ConditionalExpression
	;
%%
int yyerror(char *msg){
	printf("error in line %d: %s\n",yylineno,msg);
	//printf("LINE:%d",line);
	return 1;
}		
void main(){
	//printf("Enter the expression\n");
	yyin=fopen("in1.txt","r");
	do{
		if(yyparse()){
			printf("Failure\n");
			exit(0);
		}
	}while(!feof(yyin));
	printf("Success!\n");
	printf("SYmbol Table:\n");
	printTable();
}
