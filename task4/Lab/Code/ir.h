#ifndef IR_H
#define IR_H

#include "ast.h"
#include <stdio.h>

/* 中间代码链表节点 */
typedef struct IRCode {
    char* code;
    struct IRCode* next;
} IRCode;

/* 入口函数：遍历AST并生成中间代码到文件 */
void generateIR(ASTNode* root, const char* outputFile);

#endif
