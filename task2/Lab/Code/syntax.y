%code requires {
#include "ast.h"
}

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "semantic.h"

#define YY_DECL int yylex()
#include "lex.yy.c"

extern FILE* yyin;
extern int yyparse();
extern int yylineno;
void yyerror(const char* s);
int has_error = 0;
%}

%locations

/*语义分析栈中可以放置的节点类型*/
%union {
    int int_val;
    float float_val;
    char* str_val;
    ASTNode* node;
}

/* Tokens from Appendix.pdf */
%token <int_val> INT
%token <float_val> FLOAT
%token <str_val> ID
%token <str_val> TYPE /* int | float */
%token <str_val> RELOP /* > | < | >= | <= | == | != */
%token SEMI COMMA ASSIGNOP
%token PLUS MINUS STAR DIV
%token AND OR DOT NOT
%token LP RP LB RB LC RC
%token STRUCT RETURN IF ELSE WHILE
%token UMINUS

/* 声明token类型对应的在栈中的类型 */
%type <node> Program ExtDefList ExtDef Specifier ExtDecList VarDec
%type <node> StructSpecifier OptTag Tag
%type <node> FunDec VarList ParamDec
%type <node> CompSt DefList StmtList Stmt DecList Dec Def
%type <node> Exp Args

%debug
%start Program

/* Operator precedence */
%right ASSIGNOP
%left OR
%left AND
%left RELOP
%left PLUS MINUS
%left STAR DIV
%right NOT UMINUS
%left DOT LB
/* Dangling else resolution */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%

/* High-level Definitions */
Program
    : ExtDefList {
        $$ = createNode("Program", NULL, $1 ? $1->line : 1);
        $$->child = $1;
        root = $$;
        if (!has_error) {
            /* printf("Program parsed successfully.\n");*/
        }
    }
    ;

ExtDefList: ExtDef ExtDefList {
        $$ = createNode("ExtDefList", NULL, $1->line);
        $$->child = $1;
        if ($2) {
            $1->right = $2;  /* 兄弟节点 */
        }
    }
    | %empty { $$ = NULL; }
    ;

ExtDef
    : Specifier ExtDecList SEMI {
        int line = @3.first_line;
        $$ = createNode("ExtDef", NULL, $1->line);
        $$->child = $1;  /* Specifier */
        $$->child->right = $2;  /* ExtDecList */
        if ($2) {
            $2->right = createNode("SEMI", NULL, line);
        } else {
            $1->right = createNode("SEMI", NULL, line);
        }
    }
    | Specifier SEMI {
        int line = @2.first_line;
        $$ = createNode("ExtDef", NULL, $1->line);
        $$->child = $1;  /* Specifier */
        $1->right = createNode("SEMI", NULL, line);
    }
    | Specifier FunDec CompSt {
        $$ = createNode("ExtDef", NULL, $1->line);
        $$->child = $1;  /* Specifier */
        $$->child->right = $2;  /* FunDec */
        $2->right = $3;  /* CompSt */
    }
    | Specifier FunDec SEMI {
        $$ = createNode("ExtDef", NULL, $1->line);
        $$->child = $1;  /* Specifier */
        $$->child->right = $2;  /* FunDec */
        $2->right = createNode("SEMI", NULL, @3.first_line);
    }
    | error SEMI { $$ = createNode("ExtDef", NULL, @1.first_line); }
    ;

Specifier
    : TYPE {
        int line = @1.first_line;
        $$ = createNode("Specifier", NULL, line);
        $$->child = createNode("TYPE", $1, line);
    }
    | StructSpecifier {
        $$ = createNode("Specifier", NULL, $1->line);
        $$->child = $1;
    }
    ;

StructSpecifier
    : STRUCT OptTag LC DefList RC {
        int line = @1.first_line;
        $$ = createNode("StructSpecifier", NULL, line);
        $$->child = createNode("STRUCT", NULL, line);
        ASTNode* lc = createNode("LC", NULL, @3.first_line);
        ASTNode* rc = createNode("RC", NULL, @5.first_line);

        if ($2) {
            $$->child->right = $2;
            $2->right = lc;
        } else {
            $$->child->right = lc;
        }

        if ($4) {
            lc->right = $4;
            $4->right = rc;
        } else {
            lc->right = rc;
        }
    }
    | STRUCT Tag {
        int line = @1.first_line;
        $$ = createNode("StructSpecifier", NULL, line);
        $$->child = createNode("STRUCT", NULL, line);
        $$->child->right = $2;
    }
    | STRUCT OptTag LC error RC {
        $$ = createNode("StructSpecifier", NULL, @1.first_line);
    }
    ;

OptTag
    : ID {
        int line = @1.first_line;
        $$ = createNode("OptTag", NULL, line);
        $$->child = createNode("ID", $1, line);
    }
    | %empty { $$ = NULL; }
    ;

Tag
    : ID {
        int line = @1.first_line;
        $$ = createNode("Tag", NULL, line);
        $$->child = createNode("ID", $1, line);
    }
    ;

ExtDecList
    : VarDec {
        $$ = createNode("ExtDecList", NULL, $1->line);
        $$->child = $1;
    }
    | VarDec COMMA ExtDecList {
        $$ = createNode("ExtDecList", NULL, $1->line);
        $$->child = $1;
        ASTNode* comma = createNode("COMMA", NULL, @2.first_line);
        $1->right = comma;
        comma->right = $3;
    }
    ;

VarDec
    : ID {
        int line = @1.first_line;
        $$ = createNode("VarDec", NULL, line);
        $$->child = createNode("ID", $1, line);
    }
    | VarDec LB INT RB {
        int line = @3.first_line;
        $$ = createNode("VarDec", NULL, $1->line);
        $$->child = $1;
        ASTNode* lb = createNode("LB", NULL, @2.first_line);
        ASTNode* intNode = createNode("INT", NULL, line);
        sprintf(intNode->value, "%d", $3);
        ASTNode* rb = createNode("RB", NULL, @4.first_line);
        $1->right = lb;
        lb->right = intNode;
        intNode->right = rb;
    }
    ;

FunDec
    : ID LP VarList RP {
        int line = @1.first_line;
        $$ = createNode("FunDec", NULL, line);
        $$->child = createNode("ID", $1, line);
        ASTNode* lp = createNode("LP", NULL, @2.first_line);
        ASTNode* rp = createNode("RP", NULL, @4.first_line);
        $$->child->right = lp;
        lp->right = $3;
        $3->right = rp;
    }
    | ID LP RP {
        int line = @1.first_line;
        $$ = createNode("FunDec", NULL, line);
        $$->child = createNode("ID", $1, line);
        ASTNode* lp = createNode("LP", NULL, @2.first_line);
        ASTNode* rp = createNode("RP", NULL, @3.first_line);
        $$->child->right = lp;
        lp->right = rp;
    }
    ;

VarList
    : ParamDec COMMA VarList {
        $$ = createNode("VarList", NULL, $1->line);
        $$->child = $1;
        ASTNode* comma = createNode("COMMA", NULL, @2.first_line);
        $1->right = comma;
        comma->right = $3;
    }
    | ParamDec {
        $$ = createNode("VarList", NULL, $1->line);
        $$->child = $1;
    }
    ;

ParamDec
    : Specifier VarDec {
        $$ = createNode("ParamDec", NULL, $1->line);
        $$->child = $1;  /* Specifier */
        $1->right = $2;  /* VarDec */
    }
    ;

/* Local Definitions */
CompSt
    : LC DefList StmtList RC {
        int line = @1.first_line;
        $$ = createNode("CompSt", NULL, line);
        ASTNode* lc = createNode("LC", NULL, line);
        ASTNode* rc = createNode("RC", NULL, @4.first_line);

        $$->child = lc;
        if ($2) {
            lc->right = $2;
            if ($3) {
                $2->right = $3;
                $3->right = rc;
            } else {
                $2->right = rc;
            }
        } else if ($3) {
            lc->right = $3;
            $3->right = rc;
        } else {
            lc->right = rc;
        }
    }
    ;

DefList
    : Def DefList {
        $$ = createNode("DefList", NULL, $1->line);
        $$->child = $1;
        if ($2) {
            $1->right = $2;  /* 兄弟节点 */
        }
    }
    | %empty { $$ = NULL; }
    ;

Def
    : Specifier DecList SEMI {
        int line = @3.first_line;
        $$ = createNode("Def", NULL, $1->line);
        $$->child = $1;  /* Specifier */
        $1->right = $2;  /* DecList */
        if ($2) {
            $2->right = createNode("SEMI", NULL, line);
        } else {
            $1->right = createNode("SEMI", NULL, line);
        }
    }
    | Specifier error SEMI { $$ = createNode("Def", NULL, $1->line); }
    ;

DecList
    : Dec {
        $$ = createNode("DecList", NULL, $1->line);
        $$->child = $1;
    }
    | Dec COMMA DecList {
        $$ = createNode("DecList", NULL, $1->line);
        $$->child = $1;
        ASTNode* comma = createNode("COMMA", NULL, @2.first_line);
        $1->right = comma;
        comma->right = $3;
    }
    ;

Dec
    : VarDec {
        $$ = createNode("Dec", NULL, $1->line);
        $$->child = $1;
    }
    | VarDec ASSIGNOP Exp {
        $$ = createNode("Dec", NULL, $1->line);
        $$->child = $1;  /* VarDec */
        ASTNode* assignop = createNode("ASSIGNOP", NULL, @2.first_line);
        $1->right = assignop;
        assignop->right = $3;  /* Exp */
    }
    ;

/* Statements */
StmtList
    : Stmt StmtList {
        $$ = createNode("StmtList", NULL, $1->line);
        $$->child = $1;
        if ($2) {
            $1->right = $2;  /* 兄弟节点 */
        }
    }
    | %empty { $$ = NULL; }
    ;

Stmt
    : Exp SEMI {
        int line = @2.first_line;
        $$ = createNode("Stmt", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("SEMI", NULL, line);
    }
    | CompSt {
        $$ = createNode("Stmt", NULL, $1->line);
        $$->child = $1;
    }
    | RETURN Exp SEMI {
        int line = @1.first_line;
        int semi_line = @3.first_line;
        $$ = createNode("Stmt", NULL, line);
        $$->child = createNode("RETURN", NULL, line);
        $$->child->right = $2;  /* Exp */
        $2->right = createNode("SEMI", NULL, semi_line);
    }
    | IF LP Exp RP Stmt %prec LOWER_THAN_ELSE {
        int line = @1.first_line;
        $$ = createNode("Stmt", NULL, line);
        ASTNode* if_node = createNode("IF", NULL, line);
        ASTNode* lp = createNode("LP", NULL, @2.first_line);
        ASTNode* rp = createNode("RP", NULL, @4.first_line);
        $$->child = if_node;
        if_node->right = lp;
        lp->right = $3;  /* Exp */
        $3->right = rp;
        rp->right = $5;  /* Stmt */
    }
    | IF LP Exp RP Stmt ELSE Stmt {
        int line = @1.first_line;
        $$ = createNode("Stmt", NULL, line);
        ASTNode* if_node = createNode("IF", NULL, line);
        ASTNode* lp = createNode("LP", NULL, @2.first_line);
        ASTNode* rp = createNode("RP", NULL, @4.first_line);
        ASTNode* else_node = createNode("ELSE", NULL, @6.first_line);
        $$->child = if_node;
        if_node->right = lp;
        lp->right = $3;  /* Exp */
        $3->right = rp;
        rp->right = $5;  /* then Stmt */
        $5->right = else_node;
        else_node->right = $7;  /* else Stmt */
    }
    | WHILE LP Exp RP Stmt {
        int line = @1.first_line;
        $$ = createNode("Stmt", NULL, line);
        ASTNode* while_node = createNode("WHILE", NULL, line);
        ASTNode* lp = createNode("LP", NULL, @2.first_line);
        ASTNode* rp = createNode("RP", NULL, @4.first_line);
        $$->child = while_node;
        while_node->right = lp;
        lp->right = $3;  /* Exp */
        $3->right = rp;
        rp->right = $5;  /* Stmt */
    }
    | error SEMI { $$ = createNode("Stmt", NULL, @1.first_line); }
    | error RC { $$ = createNode("Stmt", NULL, @1.first_line); }
    | WHILE LP error RP Stmt {
        $$ = createNode("Stmt", NULL, @1.first_line);
    }
    | IF LP error RP Stmt %prec LOWER_THAN_ELSE {
        $$ = createNode("Stmt", NULL, @1.first_line);
    }
    | IF LP error RP Stmt ELSE Stmt {
        $$ = createNode("Stmt", NULL, @1.first_line);
    }
    ;

/* Expressions */
Exp
    : Exp ASSIGNOP Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("ASSIGNOP", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | Exp AND Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("AND", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | Exp OR Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("OR", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | Exp RELOP Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("RELOP", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | Exp PLUS Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("PLUS", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | Exp MINUS Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("MINUS", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | Exp STAR Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("STAR", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | Exp DIV Exp {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("DIV", NULL, @2.first_line);
        $1->right->right = $3;
    }
    | LP Exp RP {
        $$ = createNode("Exp", NULL, @1.first_line);
        ASTNode* lp = createNode("LP", NULL, @1.first_line);
        ASTNode* rp = createNode("RP", NULL, @3.first_line);
        $$->child = lp;
        lp->right = $2;
        $2->right = rp;
    }
    | MINUS Exp %prec UMINUS {
        int line = @1.first_line;
        $$ = createNode("Exp", NULL, line);
        $$->child = createNode("MINUS", NULL, line);
        $$->child->right = $2;
    }
    | NOT Exp {
        int line = @1.first_line;
        $$ = createNode("Exp", NULL, line);
        $$->child = createNode("NOT", NULL, line);
        $$->child->right = $2;
    }
    | ID LP Args RP {
        int line = @1.first_line;
        $$ = createNode("Exp", NULL, line);
        $$->child = createNode("ID", $1, line);
        $$->child->right = createNode("LP", NULL, @2.first_line);
        $$->child->right->right = $3;  /* Args */
        $3->right = createNode("RP", NULL, @4.first_line);
    }
    | ID LP RP {
        int line = @1.first_line;
        $$ = createNode("Exp", NULL, line);
        $$->child = createNode("ID", $1, line);
        $$->child->right = createNode("LP", NULL, @2.first_line);
        $$->child->right->right = createNode("RP", NULL, @3.first_line);
    }
    | Exp LB Exp RB {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("LB", NULL, @2.first_line);
        $1->right->right = $3;  /* index Exp */
        $3->right = createNode("RB", NULL, @4.first_line);
    }
    | Exp DOT ID {
        $$ = createNode("Exp", NULL, $1->line);
        $$->child = $1;
        $1->right = createNode("DOT", NULL, @2.first_line);
        $1->right->right = createNode("ID", $3, @3.first_line);
    }
    | ID {
        int line = @1.first_line;
        $$ = createNode("Exp", NULL, line);
        $$->child = createNode("ID", $1, line);
    }
    | INT {
        int line = @1.first_line;
        $$ = createNode("Exp", NULL, line);
        $$->child = createNode("INT", NULL, line);
        sprintf($$->child->value, "%d", $1);
    }
    | FLOAT {
        int line = @1.first_line;
        $$ = createNode("Exp", NULL, line);
        $$->child = createNode("FLOAT", NULL, line);
        sprintf($$->child->value, "%f", $1);
    }
    ;

Args
    : Exp COMMA Args {
        $$ = createNode("Args", NULL, $1->line);
        $$->child = $1;
        ASTNode* comma = createNode("COMMA", NULL, @2.first_line);
        $1->right = comma;
        comma->right = $3;
    }
    | Exp {
        $$ = createNode("Args", NULL, $1->line);
        $$->child = $1;
    }
    ;

%%

void yyerror(const char* s) {
    /* 默认错误消息改进 */
    has_error = 1;
    if (strstr(s, "syntax error") != NULL) {
        printf("Error type B at Line %d: syntax error.\n", yylineno);
    } else {
        printf("Error type B at Line %d: %s\n", yylineno, s);
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* input = fopen(argv[1], "r");
    if (!input) {
        fprintf(stderr, "Error opening file: %s\n", argv[1]);
        return 1;
    }

    yyin = input;

    if (yyparse() == 0) {
        if (!has_error) {
            semanticAnalysis(root);
        }
        freeAST(root);
    } else {
        freeAST(root);
    }

    fclose(input);
    return 0;
}