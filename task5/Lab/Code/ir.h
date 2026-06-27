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

/* 从文本IR文件读取IRCode链表 */
IRCode* readIRFromFile(const char* filename);

/* 输出IRCode链表到文件（或stdout） */
void outputIR(IRCode* ir, const char* outputFile);

/* 释放IRCode链表 */
void freeIR(IRCode* ir);

#endif
