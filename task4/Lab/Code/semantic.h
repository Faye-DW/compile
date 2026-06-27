#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "ast.h"

/* Entry point: walk AST and report all semantic errors */
void semanticAnalysis(ASTNode* root);

#endif /* SEMANTIC_H */
