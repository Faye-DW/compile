%{
#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L
#define YY_NO_UNISTD_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "syntax.tab.h"
#include "utils.h"

extern YYSTYPE yylval;
extern int has_error;

#define YY_USER_ACTION \
    yylloc.first_line = yylloc.last_line = yylineno; \
    yylloc.first_column = yylloc.last_column;


%}

%option noyywrap
%option yylineno
%option never-interactive
/* opyion yylineno*/

/* 空白字符 */
WHITESPACE [ \t\n\r]+

/* 注释 */
COMMENT \/\*[^*]*\*+([^/*][^*]*\*+)*\/
LINECOMMENT \/\/[^\n]*

/* 关键字 */
TYPE_INT int
TYPE_FLOAT float
STRUCT struct
RETURN return
IF if
ELSE else
WHILE while

/* 标识符 */
ID [a-zA-Z_][a-zA-Z0-9_]*

/* 数字 - 完整支持十进制、八进制、十六进制 */
DECIMAL [1-9][0-9]*|0
OCTAL 0[0-7]+
HEXADECIMAL 0[xX][0-9a-fA-F]+

/* 错误数字格式 */
ERR_OCTAL 0[0-9]+
ERR_HEX 0[xX][0-9a-zA-Z]+

/* 浮点数 - 支持小数点和科学计数法 */
FLOATING1 [0-9]+\.[0-9]*
EXPONENT [eE][+-]?[0-9]+
FLOATING {FLOATING1}{EXPONENT}?
FLOATING_EXP [0-9]+{EXPONENT}

/* 运算符 */
ASSIGNOP =
RELOP >|<|>=|<=|==|!=
PLUS \+
MINUS -
STAR \*
DIV \/
AND &&
OR \|\|
DOT \.
NOT !

/* 分隔符 */
SEMI ;
COMMA ,
LP \(
RP \)
LB \[
RB \]
LC \{
RC \}

%%

{WHITESPACE} { /* 忽略空白 */ }
{COMMENT} { /* 忽略多行注释 */ }
{LINECOMMENT} { /* 忽略单行注释 */ }

{TYPE_INT} { yylval.str_val = my_strdup("int"); return TYPE; }
{TYPE_FLOAT} { yylval.str_val = my_strdup("float"); return TYPE; }
{STRUCT} { return STRUCT; }
{RETURN} { return RETURN; }
{IF} { return IF; }
{ELSE} { return ELSE; }
{WHILE} { return WHILE; }

{ID} {
    /* 检查标识符长度不超过32字符 */
    if (strlen(yytext) > 32) {
        printf("Error type A at Line %d: Identifier '%s' exceeds max length of 32.\n", yylineno, yytext);
        has_error = 1;
        // Continue processing, return ID anyway
    }
    yylval.str_val = my_strdup(yytext);
    return ID;
}

{DECIMAL} {
    /* 十进制整数 */
    yylval.int_val = atoi(yytext);
    return INT;
}

{OCTAL} {
    /* 八进制整数 */
    char *endptr;
    yylval.int_val = strtol(yytext, &endptr, 8);
    return INT;
}

{HEXADECIMAL} {
    /* 十六进制整数 */
    char *endptr;
    yylval.int_val = strtol(yytext, &endptr, 16);
    return INT;
}

{FLOATING} {
    /* 浮点数 */
    yylval.float_val = atof(yytext);
    return FLOAT;
}

{FLOATING_EXP} {
    /* 整数加科学计数法，如 1e5 */
    yylval.float_val = atof(yytext);
    return FLOAT;
}

{ERR_OCTAL} {
    /* Check if it contains 8 or 9 (invalid octal digits) */
    int is_invalid = 0;
    for (int i = 1; yytext[i]; i++) {
        if (yytext[i] == '8' || yytext[i] == '9') {
            is_invalid = 1;
            break;
        }
    }
    if (is_invalid) {
        printf("Error type A at Line %d: Illegal octal number '%s'.\n", yylineno, yytext);
        has_error = 1;
        yylval.int_val = 0;
        return INT;
    } else {
        /* Valid octal */
        char *endptr;
        yylval.int_val = strtol(yytext, &endptr, 8);
        return INT;
    }
}

{ERR_HEX} {
    /* Check if all chars after 0x are valid hex digits */
    int is_invalid = 0;
    for (int i = 2; yytext[i]; i++) {
        char c = yytext[i];
        if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
            is_invalid = 1;
            break;
        }
    }
    if (is_invalid) {
        printf("Error type A at Line %d: Illegal hexadecimal number '%s'.\n", yylineno, yytext);
        has_error = 1;
        yylval.int_val = 0;
        return INT;
    } else {
        /* Valid hex */
        char *endptr;
        yylval.int_val = strtol(yytext, &endptr, 16);
        return INT;
    }
}

{ASSIGNOP} { return ASSIGNOP; }
{RELOP} {
    yylval.str_val = my_strdup(yytext);
    return RELOP;
}
{PLUS} { return PLUS; }
{MINUS} { return MINUS; }
{STAR} { return STAR; }
{DIV} { return DIV; }
{AND} { return AND; }
{OR} { return OR; }
{DOT} { return DOT; }
{NOT} { return NOT; }

{SEMI} { return SEMI; }
{COMMA} { return COMMA; }
{LP} { return LP; }
{RP} { return RP; }
{LB} { return LB; }
{RB} { return RB; }
{LC} { return LC; }
{RC} { return RC; }

. {
    printf("Error type A at Line %d: Mysterious character '%c'.\n", yylineno, yytext[0]);
    has_error = 1;
    // Skip this character, continue scanning
}

%%