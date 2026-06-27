#define _POSIX_C_SOURCE 200809L
#include "ast.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* 全局根节点定义 */
ASTNode* root = NULL;

ASTNode* createNode(const char* type, const char* value, int line) {
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    if (!node) {
        fprintf(stderr, "Memory allocation error\n");
        return NULL;
    }
    node->type = my_strdup(type);
    if (value) {
        node->value = my_strdup(value);
    } else {
        /* 分配默认空间，稍后可以填充 */
        node->value = malloc(64);
        if (node->value) {
            node->value[0] = '\0';
        } else {
            node->value = NULL;
        }
    }
    node->line = line;
    node->left = NULL;
    node->right = NULL;
    node->child = NULL;
    return node;
}

void printAST(ASTNode* node, int depth) {
    if (!node) return;

    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
    printf("%s", node->type);

    /* 判断是否为全大写token（除了INT/FLOAT/ID/TYPE） */
    int is_token = 1;
    const char* p = node->type;
    while (*p) {
        if (!isupper((unsigned char)*p) && !isdigit((unsigned char)*p)) {
            is_token = 0;
            break;
        }
        p++;
    }

    /* 对于需要打印value的token: INT, FLOAT, ID, TYPE */
    if (strcmp(node->type, "INT") == 0 || strcmp(node->type, "FLOAT") == 0 ||
        strcmp(node->type, "ID") == 0 || strcmp(node->type, "TYPE") == 0) {
        if (node->value && node->value[0] != '\0') {
            printf(": %s", node->value);
        }
        printf("\n");
    } else if (is_token) {
        /* 其他全大写token不打印value或行号 */
        printf("\n");
    } else if (node->value && node->value[0] != '\0') {
        /* 非终结符有value（通常没有） */
        printf(" (%d)", node->line);
        printf("\n");
    } else {
        /* 非终结符没有value，打印行号 */
        printf(" (%d)", node->line);
        printf("\n");
    }

    printAST(node->child, depth + 1);
    printAST(node->left, depth);
    printAST(node->right, depth);
}

void freeAST(ASTNode* node) {
    if (!node) return;
    freeAST(node->child);
    freeAST(node->left);
    freeAST(node->right);
    free(node->type);
    free(node->value);
    free(node);
}

void printLeafNodesPreorder(ASTNode* node) {
    if (!node) return;

    /* 先序遍历：先访问当前节点 */
    /* 打印所有节点，不仅仅是叶子节点 */
    printf("%s", node->type);

    /* 判断是否为全大写token（除了INT/FLOAT/ID/TYPE） */
    int is_token = 1;
    const char* p = node->type;
    while (*p) {
        if (!isupper((unsigned char)*p) && !isdigit((unsigned char)*p)) {
            is_token = 0;
            break;
        }
        p++;
    }

    /* 对于需要打印value的token: INT, FLOAT, ID, TYPE */
    if (strcmp(node->type, "INT") == 0 || strcmp(node->type, "FLOAT") == 0 ||
        strcmp(node->type, "ID") == 0 || strcmp(node->type, "TYPE") == 0) {
        if (node->value && node->value[0] != '\0') {
            printf(": %s", node->value);
        }
        printf("\n");
    } else if (is_token) {
        /* 其他全大写token不打印value */
        printf("\n");
    } else if (node->value && node->value[0] != '\0') {
        /* 非终结符有value（通常没有） */
        printf("\n");
    } else {
        printf("\n");
    }

    /* 然后递归遍历子节点（child） */
    printLeafNodesPreorder(node->child);
    /* 最后递归遍历兄弟节点（right） */
    printLeafNodesPreorder(node->right);
}