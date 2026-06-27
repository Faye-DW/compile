#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 语法树节点结构 */
typedef struct ASTNode {
    char* type;        /* 节点类型 */
    char* value;       /* 节点值（用于标识符、数字等） */
    int line;          /* 行号 */
    struct ASTNode* left;
    struct ASTNode* right;
    struct ASTNode* child; /* 用于多子节点（如语句列表） 第一个孩子*/
} ASTNode;

/* 全局根节点声明 */
extern ASTNode* root;

/* 创建新节点
 * 参数: type - 节点类型字符串，value - 节点值字符串（可为NULL），line - 行号
 * 返回: 新分配的节点指针，调用者负责释放
 */
ASTNode* createNode(const char* type, const char* value, int line);

/* 打印AST树
 * 参数: node - 根节点，depth - 当前深度（调用时传0）
 */
void printAST(ASTNode* node, int depth);

/* 递归释放AST树内存 */
void freeAST(ASTNode* node);

/* 先序遍历打印叶子节点 */
void printLeafNodesPreorder(ASTNode* node);

#endif /* AST_H */