#define _POSIX_C_SOURCE 200809L
#include "ir.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* ================================================================
 * Helpers: temporaries and labels
 * ================================================================ */

static int tempCnt = 0;
static int labelCnt = 0;
static int varCnt = 0;

static char* new_temp(void) {
    char buf[32];
    snprintf(buf, sizeof(buf), "t%d", ++tempCnt);
    return my_strdup(buf);
}

static char* new_label(void) {
    char buf[32];
    snprintf(buf, sizeof(buf), "label%d", ++labelCnt);
    return my_strdup(buf);
}

static char* new_var(void) {
    char buf[32];
    snprintf(buf, sizeof(buf), "v%d", ++varCnt);
    return my_strdup(buf);
}

/* ================================================================
 * IRCode linked list primitives
 * ================================================================ */

static IRCode* createCode(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    char buf[256];
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    IRCode* c = (IRCode*)malloc(sizeof(IRCode));
    c->code = my_strdup(buf);
    c->next = NULL;
    return c;
}

static IRCode* concatCodes(IRCode* a, IRCode* b) {
    if (!a) return b;
    if (!b) return a;
    IRCode* p = a;
    while (p->next) p = p->next;
    p->next = b;
    return a;
}

static void printIR(IRCode* ir, FILE* out) {
    while (ir) {
        fprintf(out, "%s\n", ir->code);
        ir = ir->next;
    }
}

static void freeIR(IRCode* ir) {
    while (ir) {
        IRCode* next = ir->next;
        free(ir->code);
        free(ir);
        ir = next;
    }
}

/* ================================================================
 * AST helpers
 * ================================================================ */

static int nodeIs(ASTNode* n, const char* t) {//判断节点存在 && 类型和ast树上是匹配的
    return n && strcmp(n->type, t) == 0;
}

static const char* opSymbol(const char* type) {
    if (strcmp(type, "PLUS") == 0)  return "+";
    if (strcmp(type, "MINUS") == 0) return "-";
    if (strcmp(type, "STAR") == 0)  return "*";
    if (strcmp(type, "DIV") == 0)   return "/";
    return type;
}

static char* getVarDecName(ASTNode* node) {
    if (!node || !nodeIs(node, "VarDec")) return NULL;
    ASTNode* child = node->child;
    if (nodeIs(child, "ID")) return child->value;
    if (nodeIs(child, "VarDec")) return getVarDecName(child);
    return NULL;
}

static char* getExpVarName(ASTNode* exp) {
    if (!exp || !nodeIs(exp, "Exp")) return NULL;
    ASTNode* c = exp->child;
    if (nodeIs(c, "ID") && !c->right) return c->value;
    return NULL;
}

/* ================================================================
 * Type tracking for struct / array IR generation
 * ================================================================ */

typedef enum { IRK_INT, IRK_FLOAT, IRK_STRUCT, IRK_ARRAY } IRTypeKind;

typedef struct IRField_ {
    char*            name;
    int              offset;
    struct IRType_*  type;
    struct IRField_* next;
} IRField;

typedef struct IRType_ {
    IRTypeKind kind;
    int        size;
    IRField*   fields;
    struct IRType_* elem;
    int        elemCount;
} IRType;

typedef struct IRVar_ {
    char*            name;      /* original C name */
    char*            irName;    /* vN name used in IR output */
    IRType*          type;
    int              isAddr;
    struct IRVar_*   next;
} IRVar;

static IRVar* irVars     = NULL;
static IRVar* irStructs  = NULL;

static IRType* irInt(void) {
    static IRType t = {IRK_INT, 4, NULL, NULL, 0};
    return &t;
}
static IRType* irFloat(void) {
    static IRType t = {IRK_FLOAT, 4, NULL, NULL, 0};
    return &t;
}
static IRType* irArray(IRType* elem, int count) {
    IRType* t = malloc(sizeof(IRType));
    t->kind = IRK_ARRAY;
    t->size = elem->size * count;
    t->fields = NULL;
    t->elem = elem;
    t->elemCount = count;
    return t;
}
static IRType* irStruct(IRField* fields, int size) {
    IRType* t = malloc(sizeof(IRType));
    t->kind = IRK_STRUCT;
    t->size = size;
    t->fields = fields;
    t->elem = NULL;
    t->elemCount = 0;
    return t;
}

static void irRegStruct(const char* name, IRType* type) {
    IRVar* s = malloc(sizeof(IRVar));
    s->name   = my_strdup(name);
    s->irName = my_strdup(name);
    s->type   = type;
    s->isAddr = 0;
    s->next   = irStructs;
    irStructs = s;
}
static IRType* irLookupStruct(const char* name) {
    for (IRVar* s = irStructs; s; s = s->next)
        if (strcmp(s->name, name) == 0) return s->type;
    return NULL;
}

static void irRegVar(const char* name, const char* irName, IRType* type, int isAddr) {
    IRVar* v = malloc(sizeof(IRVar));
    v->name   = my_strdup(name);
    v->irName = my_strdup(irName);
    v->type   = type;
    v->isAddr = isAddr;
    v->next   = irVars;
    irVars    = v;
}
static IRVar* irLookupVar(const char* name) {
    for (IRVar* v = irVars; v; v = v->next)
        if (strcmp(v->name, name) == 0) return v;
    return NULL;
}
static int irFieldOffset(IRType* st, const char* fname) {
    if (!st || st->kind != IRK_STRUCT) return 0;
    for (IRField* f = st->fields; f; f = f->next)
        if (strcmp(f->name, fname) == 0) return f->offset;
    return 0;
}

static IRType* irVarDecType(ASTNode* varDec, IRType* base) {
    if (!varDec || !nodeIs(varDec, "VarDec")) return base;
    ASTNode* child = varDec->child;
    if (nodeIs(child, "ID")) return base;
    ASTNode* lb      = child ? child->right : NULL;
    ASTNode* intNode = lb ? lb->right : NULL;
    int size = (intNode && nodeIs(intNode, "INT")) ? atoi(intNode->value) : 0;
    IRType* inner = irVarDecType(child, base);
    return irArray(inner, size);
}

static IRType* irBuildStruct(ASTNode* defListNode) {
    IRField* head = NULL;
    IRField** tail = &head;
    int totalSize = 0;

    ASTNode* dl = defListNode;
    while (dl && nodeIs(dl, "DefList")) {
        ASTNode* def = dl->child;
        if (!def) break;

        ASTNode* spec    = def->child;
        ASTNode* decList = spec ? spec->right : NULL;

        IRType* base = NULL;
        ASTNode* specChild = spec ? spec->child : NULL;
        if (nodeIs(specChild, "TYPE")) {
            base = (strcmp(specChild->value, "int") == 0) ? irInt() : irFloat();
        } else if (nodeIs(specChild, "StructSpecifier")) {
            ASTNode* ssChild = specChild->child;
            ASTNode* next = ssChild ? ssChild->right : NULL;
            if (nodeIs(next, "Tag")) {
                const char* tn = next->child ? next->child->value : NULL;
                base = tn ? irLookupStruct(tn) : NULL;
                if (!base) base = irInt();
            } else {
                ASTNode* lc = next;
                if (nodeIs(next, "OptTag")) lc = next->right;
                ASTNode* body = lc ? lc->right : NULL;
                base = body ? irBuildStruct(body) : irInt();
            }
        }
        if (!base) { dl = def->right; continue; }

        ASTNode* dcl = decList;
        while (dcl && nodeIs(dcl, "DecList")) {
            ASTNode* dec    = dcl->child;
            ASTNode* varDec = dec ? dec->child : NULL;

            IRType* ftype = irVarDecType(varDec, base);
            char*   fname = getVarDecName(varDec);

            if (fname) {
                IRField* fld = malloc(sizeof(IRField));
                fld->name   = my_strdup(fname);
                fld->offset = totalSize;
                fld->type   = ftype;
                fld->next   = NULL;
                *tail = fld;
                tail  = &fld->next;
                totalSize += ftype->size;
            }

            if (dec && dec->right && nodeIs(dec->right, "COMMA"))
                dcl = dec->right->right;
            else
                dcl = NULL;
        }
        dl = def->right;
    }
    return irStruct(head, totalSize);
}

static IRType* irSpecType(ASTNode* spec) {
    if (!spec || !nodeIs(spec, "Specifier")) return irInt();
    ASTNode* child = spec->child;
    if (nodeIs(child, "TYPE"))
        return (strcmp(child->value, "int") == 0) ? irInt() : irFloat();
    if (nodeIs(child, "StructSpecifier")) {
        ASTNode* ssChild = child->child;
        ASTNode* next = ssChild ? ssChild->right : NULL;
        if (nodeIs(next, "Tag")) {
            const char* tn = next->child ? next->child->value : NULL;
            IRType* t = tn ? irLookupStruct(tn) : NULL;
            return t ? t : irInt();
        }
        ASTNode* lc = next;
        if (nodeIs(next, "OptTag")) lc = next->right;
        ASTNode* body = lc ? lc->right : NULL;
        return body ? irBuildStruct(body) : irInt();
    }
    return irInt();
}

static int irTypeIsAggregate(IRType* t) {
    return t && (t->kind == IRK_STRUCT || t->kind == IRK_ARRAY);
}

/* ================================================================
 * Lvalue address computation
 * ================================================================ */

static IRCode* translate_LvalAddr(ASTNode* node, char** addrOut);

/* ================================================================
 * Forward declarations
 * ================================================================ */

static IRCode* translate_Exp(ASTNode* node, const char* place);
static IRCode* translate_Cond(ASTNode* node, const char* lt, const char* lf);
static IRCode* translate_Stmt(ASTNode* node);
static IRCode* translate_CompSt(ASTNode* node);
static IRCode* translate_DefList(ASTNode* node);
static IRCode* translate_StmtList(ASTNode* node);
static IRCode* translate_Args(ASTNode* node, char*** arg_list, int* arg_count);
static IRCode* translate_FunDec(ASTNode* node);
static IRCode* translate_ExtDef(ASTNode* node);
static IRCode* translate_ExtDefList(ASTNode* node);

/* ================================================================
 * Lvalue address computation
 * ================================================================ */

static IRCode* translate_LvalAddr(ASTNode* node, char** addrOut) {
    if (!node || !nodeIs(node, "Exp")) return NULL;
    ASTNode* first = node->child;
    if (!first) return NULL;

    /* ---- ID ---- */
    if (nodeIs(first, "ID") && !first->right) {
        IRVar* v = irLookupVar(first->value);
        if (!v) { *addrOut = my_strdup(first->value); return NULL; }
        if (v->isAddr) {
            *addrOut = my_strdup(v->irName);
            return NULL;
        }
        if (irTypeIsAggregate(v->type)) {
            char* t = new_temp();
            *addrOut = t;
            return createCode("%s := &%s", t, v->irName);
        }
        *addrOut = my_strdup(v->irName);
        return NULL;
    }

    /* ---- LP Exp RP ---- */
    if (nodeIs(first, "LP")) {
        return translate_LvalAddr(first->right, addrOut);
    }

    /* ---- Exp DOT ID ---- */
    if (nodeIs(first, "Exp")) {
        ASTNode* op  = first->right;
        ASTNode* rhs = op ? op->right : NULL;

        if (nodeIs(op, "DOT")) {
            char* baseAddr = NULL;
            IRCode* code1 = translate_LvalAddr(first, &baseAddr);
            if (!baseAddr) return code1;

            /* determine base type to find field offset */
            ASTNode* baseFirst = first->child;
            IRType* baseType = NULL;
            if (nodeIs(baseFirst, "ID") && !baseFirst->right) {
                IRVar* bv = irLookupVar(baseFirst->value);
                if (bv) baseType = bv->type;
            }
            int offset = 0;
            if (baseType && baseType->kind == IRK_STRUCT && rhs) {
                offset = irFieldOffset(baseType, rhs->value);
            }

            if (offset == 0) {
                *addrOut = baseAddr;
                return code1;
            }
            char* t = new_temp();
            IRCode* code2 = createCode("%s := %s + #%d", t, baseAddr, offset);
            *addrOut = t;
            free(baseAddr);
            return concatCodes(code1, code2);
        }

        /* ---- Exp LB Exp RB ---- */
        if (nodeIs(op, "LB")) {
            char* baseAddr = NULL;
            IRCode* code1 = translate_LvalAddr(first, &baseAddr);
            if (!baseAddr) return code1;

            /* Determine element size */
            ASTNode* baseFirst = first->child;
            IRType* baseType = NULL;
            if (nodeIs(baseFirst, "ID") && !baseFirst->right) {
                IRVar* bv = irLookupVar(baseFirst->value);
                if (bv) baseType = bv->type;
            }
            int elemSize = 4;
            if (baseType && baseType->kind == IRK_ARRAY) {
                elemSize = baseType->elem->size;
            }

            char* tIdx = new_temp();
            /* rhs is the index Exp; rhs->right = RB */
            IRCode* code2 = translate_Exp(rhs, tIdx);
            char* tOff = new_temp();
            IRCode* code3 = createCode("%s := %s * #%d", tOff, tIdx, elemSize);
            char* t = new_temp();
            IRCode* code4 = createCode("%s := %s + %s", t, baseAddr, tOff);

            *addrOut = t;
            free(baseAddr);
            return concatCodes(concatCodes(code1, code2),
                   concatCodes(code3, code4));
        }
    }

    return NULL;
}

/* ================================================================
 * Expressions
 * ================================================================ */

static IRCode* translate_Exp(ASTNode* node, const char* place) {
    if (!node || !nodeIs(node, "Exp")) return NULL;
    ASTNode* first = node->child;
    if (!first) return NULL;

    /* ---- INT literal ---- */
    if (nodeIs(first, "INT")) {
        if (!place) return NULL;
        return createCode("%s := #%s", place, first->value);
    }

    /* ---- FLOAT literal ---- */
    if (nodeIs(first, "FLOAT")) {
        if (!place) return NULL;
        return createCode("%s := #%s", place, first->value);
    }

    /* ---- ID (plain variable) ---- */
    if (nodeIs(first, "ID") && !first->right) {
        if (!place) return NULL;
        IRVar* v = irLookupVar(first->value);
        char* srcName = v ? v->irName : first->value;
        return createCode("%s := %s", place, srcName);
    }

    /* ---- LP Exp RP ---- */
    if (nodeIs(first, "LP")) {
        return translate_Exp(first->right, place);
    }

    /* ---- Unary MINUS ---- */
    if (nodeIs(first, "MINUS") && first->right && nodeIs(first->right, "Exp")) {
        char* t1 = new_temp();
        IRCode* code1 = translate_Exp(first->right, t1);
        if (!place) return code1;
        IRCode* code2 = createCode("%s := #0 - %s", place, t1);
        return concatCodes(code1, code2);
    }

    /* ---- Unary NOT ---- */
    if (nodeIs(first, "NOT")) {
        char* lt = new_label();
        char* lf = new_label();
        IRCode* code0 = place ? createCode("%s := #0", place) : NULL;
        IRCode* code1 = translate_Cond(node, lt, lf);
        IRCode* code2 = createCode("LABEL %s :", lt);
        IRCode* code3 = place ? createCode("%s := #1", place) : NULL;
        IRCode* code4 = createCode("LABEL %s :", lf);
        IRCode* r = concatCodes(code0, code1);
        r = concatCodes(r, code2);
        r = concatCodes(r, code3);
        r = concatCodes(r, code4);
        return r;
    }

    /* ---- Function call: ID LP ... RP ---- */
    if (nodeIs(first, "ID") && first->right && nodeIs(first->right, "LP")) {
        char* fname = first->value;
        ASTNode* lp = first->right;
        ASTNode* afterLp = lp->right;

        if (afterLp && nodeIs(afterLp, "RP")) {
            if (strcmp(fname, "read") == 0) {
                if (!place) {
                    char* t = new_temp();
                    return createCode("READ %s", t);
                }
                return createCode("READ %s", place);
            }
            if (!place) {
                char* t = new_temp();
                return createCode("%s := CALL %s", t, fname);
            }
            return createCode("%s := CALL %s", place, fname);
        }

        if (afterLp && nodeIs(afterLp, "Args")) {
            char** arg_list = NULL;
            int arg_count = 0;
            IRCode* code1 = translate_Args(afterLp, &arg_list, &arg_count);

            if (strcmp(fname, "write") == 0 && arg_count > 0) {
                IRCode* code2 = createCode("WRITE %s", arg_list[0]);
                IRCode* code3 = place ? createCode("%s := #0", place) : NULL;
                free(arg_list);
                return concatCodes(concatCodes(code1, code2), code3);
            }

            IRCode* code2 = NULL;
            for (int i = 0; i < arg_count; i++) {
                code2 = concatCodes(code2, createCode("ARG %s", arg_list[i]));
            }
            IRCode* code3 = place ? createCode("%s := CALL %s", place, fname)
                                  : createCode("CALL %s", fname);
            free(arg_list);
            return concatCodes(concatCodes(code1, code2), code3);
        }
    }

    /* ---- Binary: first child is Exp ---- */
    if (nodeIs(first, "Exp")) {
        ASTNode* op  = first->right;
        ASTNode* rhs = op ? op->right : NULL;
        if (!op) return NULL;

        /* --- ASSIGNOP --- */
        if (nodeIs(op, "ASSIGNOP")) {
            /* Check if left side is DOT or LB (struct/array element assign) */
            ASTNode* leftFirst = first->child;
            if (nodeIs(leftFirst, "Exp")) {
                ASTNode* leftOp = leftFirst->right;
                if (leftOp && (nodeIs(leftOp, "DOT") || nodeIs(leftOp, "LB"))) {
                    /* struct/array element assignment */
                    char* addr = NULL;
                    IRCode* code1 = translate_LvalAddr(first, &addr);
                    char* t1 = new_temp();
                    IRCode* code2 = translate_Exp(rhs, t1);
                    IRCode* code3 = createCode("*%s := %s", addr, t1);
                    IRCode* code4 = place ? createCode("%s := %s", place, t1) : NULL;
                    free(addr);
                    return concatCodes(concatCodes(concatCodes(code1, code2), code3), code4);
                }
            }

            /* Simple variable assignment */
            char* varName = getExpVarName(first);
            if (varName) {
                IRVar* v = irLookupVar(varName);
                char* dest = v ? v->irName : varName;
                char* t1 = new_temp();
                IRCode* code1 = translate_Exp(rhs, t1);
                IRCode* code2 = createCode("%s := %s", dest, t1);
                IRCode* code3 = place ? createCode("%s := %s", place, dest) : NULL;
                return concatCodes(concatCodes(code1, code2), code3);
            }
        }

        /* --- Struct field access: Exp DOT ID (rvalue) --- */
        if (nodeIs(op, "DOT")) {
            char* addr = NULL;
            IRCode* code1 = translate_LvalAddr(node, &addr);
            if (!place) return code1;
            IRCode* code2 = createCode("%s := *%s", place, addr);
            free(addr);
            return concatCodes(code1, code2);
        }

        /* --- Array subscript: Exp LB Exp RB (rvalue) --- */
        if (nodeIs(op, "LB")) {
            char* addr = NULL;
            IRCode* code1 = translate_LvalAddr(node, &addr);
            if (!place) return code1;
            IRCode* code2 = createCode("%s := *%s", place, addr);
            free(addr);
            return concatCodes(code1, code2);
        }

        /* --- Arithmetic: PLUS MINUS STAR DIV --- */
        if (nodeIs(op, "PLUS") || nodeIs(op, "MINUS") ||
            nodeIs(op, "STAR") || nodeIs(op, "DIV")) {
            char* t1 = new_temp();
            char* t2 = new_temp();
            IRCode* code1 = translate_Exp(first, t1);
            IRCode* code2 = translate_Exp(rhs, t2);
            if (!place) {
                return concatCodes(code1, code2);
            }
            IRCode* code3 = createCode("%s := %s %s %s", place, t1,
                                       opSymbol(op->type), t2);
            return concatCodes(concatCodes(code1, code2), code3);
        }

        /* --- RELOP / AND / OR --- */
        if (nodeIs(op, "RELOP") || nodeIs(op, "AND") || nodeIs(op, "OR")) {
            char* lt = new_label();
            char* lf = new_label();
            IRCode* code0 = place ? createCode("%s := #0", place) : NULL;
            IRCode* code1 = translate_Cond(node, lt, lf);
            IRCode* code2 = createCode("LABEL %s :", lt);
            IRCode* code3 = place ? createCode("%s := #1", place) : NULL;
            IRCode* code4 = createCode("LABEL %s :", lf);
            IRCode* r = concatCodes(code0, code1);
            r = concatCodes(r, code2);
            r = concatCodes(r, code3);
            r = concatCodes(r, code4);
            return r;
        }
    }

    return NULL;
}

/* ================================================================
 * Condition expressions
 * ================================================================ */

static IRCode* translate_Cond(ASTNode* node, const char* lt, const char* lf) {
    if (!node || !nodeIs(node, "Exp")) return NULL;
    ASTNode* first = node->child;
    if (!first) return NULL;

    /* ---- NOT Exp1 ---- */
    if (nodeIs(first, "NOT")) {
        return translate_Cond(first->right, lf, lt);
    }

    /* ---- Exp RELOP Exp ---- */
    if (nodeIs(first, "Exp")) {
        ASTNode* op  = first->right;
        ASTNode* rhs = op ? op->right : NULL;

        if (op && nodeIs(op, "RELOP")) {
            char* t1 = new_temp();
            char* t2 = new_temp();
            IRCode* code1 = translate_Exp(first, t1);
            IRCode* code2 = translate_Exp(rhs, t2);
            IRCode* code3 = createCode("IF %s %s %s GOTO %s",
                                       t1, op->value, t2, lt);
            IRCode* code4 = createCode("GOTO %s", lf);
            IRCode* r = concatCodes(code1, code2);
            r = concatCodes(r, code3);
            r = concatCodes(r, code4);
            return r;
        }

        /* ---- AND ---- */
        if (op && nodeIs(op, "AND")) {
            char* l = new_label();
            IRCode* code1 = translate_Cond(first, l, lf);
            IRCode* code2 = createCode("LABEL %s :", l);
            IRCode* code3 = translate_Cond(rhs, lt, lf);
            return concatCodes(concatCodes(code1, code2), code3);
        }

        /* ---- OR ---- */
        if (op && nodeIs(op, "OR")) {
            char* l = new_label();
            IRCode* code1 = translate_Cond(first, lt, l);
            IRCode* code2 = createCode("LABEL %s :", l);
            IRCode* code3 = translate_Cond(rhs, lt, lf);
            return concatCodes(concatCodes(code1, code2), code3);
        }
    }

    /* ---- Other cases ---- */
    char* t1 = new_temp();
    IRCode* code1 = translate_Exp(node, t1);
    IRCode* code2 = createCode("IF %s != #0 GOTO %s", t1, lt);
    IRCode* code3 = createCode("GOTO %s", lf);
    return concatCodes(concatCodes(code1, code2), code3);
}

/* ================================================================
 * Arguments - handles struct/array pass-by-address
 * ================================================================ */

static IRCode* translate_Args(ASTNode* node, char*** arg_list, int* arg_count) {
    if (!node || !nodeIs(node, "Args")) return NULL;

    IRCode* code = NULL;
    int count = 0;
    char** list = NULL;

    ASTNode* cur = node;
    while (cur && nodeIs(cur, "Args")) {
        ASTNode* exp = cur->child;
        if (exp) {
            /* Check if argument is a struct/array variable */
            ASTNode* expFirst = exp->child;
            int passByAddr = 0;
            char* addrName = NULL;

            if (nodeIs(expFirst, "ID") && !expFirst->right) {
                IRVar* v = irLookupVar(expFirst->value);
                if (v && irTypeIsAggregate(v->type)) {
                    passByAddr = 1;
                    if (v->isAddr) {
                        addrName = my_strdup(v->irName);
                    } else {
                        char* t = new_temp();
                        code = concatCodes(code, createCode("%s := &%s", t, v->irName));
                        addrName = t;
                    }
                }
            }

            if (passByAddr && addrName) {
                list = (char**)realloc(list, sizeof(char*) * (count + 1));
                list[count] = addrName;
                count++;
            } else {
                char* t1 = new_temp();
                IRCode* c = translate_Exp(exp, t1);
                code = concatCodes(code, c);
                list = (char**)realloc(list, sizeof(char*) * (count + 1));
                list[count] = t1;
                count++;
            }
        }

        if (exp && exp->right && nodeIs(exp->right, "COMMA"))
            cur = exp->right->right;
        else
            cur = NULL;
    }

    *arg_list = list;
    *arg_count = count;
    return code;
}

/* ================================================================
 * Statements
 * ================================================================ */

static IRCode* translate_Stmt(ASTNode* node) {
    if (!node || !nodeIs(node, "Stmt")) return NULL;
    ASTNode* first = node->child;
    if (!first) return NULL;

    /* ---- Exp SEMI ---- */
    if (nodeIs(first, "Exp")) {
        return translate_Exp(first, NULL);
    }

    /* ---- CompSt ---- */
    if (nodeIs(first, "CompSt")) {
        return translate_CompSt(first);
    }

    /* ---- RETURN Exp SEMI ---- */
    if (nodeIs(first, "RETURN")) {
        ASTNode* exp = first->right;
        char* t1 = new_temp();
        IRCode* code1 = translate_Exp(exp, t1);
        IRCode* code2 = createCode("RETURN %s", t1);
        return concatCodes(code1, code2);
    }

    /* ---- IF LP Exp RP Stmt [ELSE Stmt] ---- */
    if (nodeIs(first, "IF")) {
        ASTNode* lp    = first->right;
        ASTNode* exp   = lp   ? lp->right  : NULL;
        ASTNode* rp    = exp  ? exp->right  : NULL;
        ASTNode* stmt1 = rp   ? rp->right   : NULL;

        char* lt = new_label();
        char* lf = new_label();
        IRCode* code1 = translate_Cond(exp, lt, lf);
        IRCode* code2 = createCode("LABEL %s :", lt);
        IRCode* code3 = translate_Stmt(stmt1);

        if (stmt1 && stmt1->right && nodeIs(stmt1->right, "ELSE")) {
            ASTNode* stmt2 = stmt1->right->right;
            char* le = new_label();
            IRCode* code4 = createCode("GOTO %s", le);
            IRCode* code5 = createCode("LABEL %s :", lf);
            IRCode* code6 = translate_Stmt(stmt2);
            IRCode* code7 = createCode("LABEL %s :", le);
            IRCode* r = concatCodes(code1, code2);
            r = concatCodes(r, code3);
            r = concatCodes(r, code4);
            r = concatCodes(r, code5);
            r = concatCodes(r, code6);
            r = concatCodes(r, code7);
            return r;
        }

        IRCode* code4 = createCode("LABEL %s :", lf);
        return concatCodes(concatCodes(concatCodes(code1, code2), code3), code4);
    }

    /* ---- WHILE LP Exp RP Stmt ---- */
    if (nodeIs(first, "WHILE")) {
        ASTNode* lp   = first->right;
        ASTNode* exp  = lp  ? lp->right  : NULL;
        ASTNode* rp   = exp ? exp->right  : NULL;
        ASTNode* stmt1 = rp  ? rp->right   : NULL;

        char* lb = new_label();
        char* lt = new_label();
        char* lf = new_label();
        IRCode* code0 = createCode("LABEL %s :", lb);
        IRCode* code1 = translate_Cond(exp, lt, lf);
        IRCode* code2 = createCode("LABEL %s :", lt);
        IRCode* code3 = translate_Stmt(stmt1);
        IRCode* code4 = createCode("GOTO %s", lb);
        IRCode* code5 = createCode("LABEL %s :", lf);
        IRCode* r = concatCodes(code0, code1);
        r = concatCodes(r, code2);
        r = concatCodes(r, code3);
        r = concatCodes(r, code4);
        r = concatCodes(r, code5);
        return r;
    }

    return NULL;
}

static IRCode* translate_StmtList(ASTNode* node) {
    if (!node || !nodeIs(node, "StmtList")) return NULL;
    IRCode* code = NULL;
    ASTNode* sl = node;
    while (sl && nodeIs(sl, "StmtList")) {
        ASTNode* stmt = sl->child;
        if (stmt) {
            code = concatCodes(code, translate_Stmt(stmt));
        }
        sl = stmt ? stmt->right : NULL;
    }
    return code;
}

/* ================================================================
 * Local definitions (DefList / DecList) - with DEC for struct/array
 * ================================================================ */

static IRCode* translate_Dec(ASTNode* node) {
    if (!node || !nodeIs(node, "Dec")) return NULL;
    ASTNode* varDec = node->child;
    if (!varDec) return NULL;

    char* name = getVarDecName(varDec);
    if (!name) return NULL;

    /* Get the var info (registered by translate_DefList before calling us) */
    IRVar* v = irLookupVar(name);
    IRCode* code = NULL;

    /* DEC for aggregate locals */
    if (v && irTypeIsAggregate(v->type)) {
        code = createCode("DEC %s %d", v->irName, v->type->size);
    }

    /* Optional initializer: VarDec ASSIGNOP Exp */
    if (varDec->right && nodeIs(varDec->right, "ASSIGNOP")) {
        ASTNode* expNode = varDec->right->right;
        if (v && irTypeIsAggregate(v->type)) {
            /* aggregate initializer is handled via separate assignment statements */
            /* for simple cases like test3, the init = {...} doesn't appear */
        } else {
            char* t1 = new_temp();
            IRCode* code1 = translate_Exp(expNode, t1);
            IRCode* code2 = createCode("%s := %s", v ? v->irName : name, t1);
            code = concatCodes(code, concatCodes(code1, code2));
        }
    }

    return code;
}

static IRCode* translate_DecList(ASTNode* node) {
    if (!node || !nodeIs(node, "DecList")) return NULL;
    IRCode* code = NULL;
    ASTNode* dcl = node;
    while (dcl && nodeIs(dcl, "DecList")) {
        ASTNode* dec = dcl->child;
        if (dec) {
            code = concatCodes(code, translate_Dec(dec));
        }
        if (dec && dec->right && nodeIs(dec->right, "COMMA"))
            dcl = dec->right->right;
        else
            dcl = NULL;
    }
    return code;
}

static IRCode* translate_Def(ASTNode* node) {
    if (!node || !nodeIs(node, "Def")) return NULL;
    ASTNode* spec = node->child;
    ASTNode* decList = spec ? spec->right : NULL;

    /* Register variable types before translating DecList */
    IRType* baseType = irSpecType(spec);
    if (decList && nodeIs(decList, "DecList")) {
        ASTNode* dcl = decList;
        while (dcl && nodeIs(dcl, "DecList")) {
            ASTNode* dec    = dcl->child;
            ASTNode* varDec = dec ? dec->child : NULL;
            char*    name   = getVarDecName(varDec);
            if (name) {
                IRType* vtype = irVarDecType(varDec, baseType);
                char* irName = new_var();
                int isAddr = 0; /* locals are values, not addresses */
                irRegVar(name, irName, vtype, isAddr);
            }
            if (dec && dec->right && nodeIs(dec->right, "COMMA"))
                dcl = dec->right->right;
            else
                dcl = NULL;
        }
    }

    if (decList && nodeIs(decList, "DecList"))
        return translate_DecList(decList);
    return NULL;
}

static IRCode* translate_DefList(ASTNode* node) {
    if (!node || !nodeIs(node, "DefList")) return NULL;
    IRCode* code = NULL;
    ASTNode* dl = node;
    while (dl && nodeIs(dl, "DefList")) {
        ASTNode* def = dl->child;
        if (def) {
            code = concatCodes(code, translate_Def(def));
        }
        dl = def ? def->right : NULL;
    }
    return code;
}

/* ================================================================
 * Compound statement
 * ================================================================ */

static IRCode* translate_CompSt(ASTNode* node) {
    if (!node || !nodeIs(node, "CompSt")) return NULL;
    ASTNode* lc  = node->child;
    ASTNode* cur = lc ? lc->right : NULL;

    IRCode* code = NULL;
    if (cur && nodeIs(cur, "DefList")) {
        code = concatCodes(code, translate_DefList(cur));
        cur = cur->right;
    }
    if (cur && nodeIs(cur, "StmtList")) {
        code = concatCodes(code, translate_StmtList(cur));
    }
    return code;
}

/* ================================================================
 * Function header (FUNCTION + PARAMs)
 * ================================================================ */

static IRCode* translate_VarList_Params(ASTNode* node) {
    if (!node || !nodeIs(node, "VarList")) return NULL;
    IRCode* code = NULL;
    ASTNode* vl = node;
    while (vl && nodeIs(vl, "VarList")) {
        ASTNode* paramDec = vl->child;
        if (paramDec) {
            ASTNode* spec   = paramDec->child;
            ASTNode* varDec = spec ? spec->right : NULL;
            if (varDec && nodeIs(varDec, "VarDec")) {
                char* name = getVarDecName(varDec);
                if (name) {
                    IRVar* v = irLookupVar(name);
                    char* irName = v ? v->irName : name;
                    code = concatCodes(code, createCode("PARAM %s", irName));
                }
            }
        }
        if (paramDec && paramDec->right && nodeIs(paramDec->right, "COMMA"))
            vl = paramDec->right->right;
        else
            vl = NULL;
    }
    return code;
}

static IRCode* translate_FunDec(ASTNode* node) {
    if (!node || !nodeIs(node, "FunDec")) return NULL;
    ASTNode* id = node->child;
    char* fname = id ? id->value : NULL;
    if (!fname) return NULL;

    IRCode* code = createCode("FUNCTION %s :", fname);

    ASTNode* lp = id->right;
    ASTNode* afterLp = lp ? lp->right : NULL;

    /* First pass: register parameters with types */
    if (afterLp && nodeIs(afterLp, "VarList")) {
        ASTNode* vl = afterLp;
        while (vl && nodeIs(vl, "VarList")) {
            ASTNode* paramDec = vl->child;
            if (paramDec) {
                ASTNode* spec   = paramDec->child;
                ASTNode* varDec = spec ? spec->right : NULL;
                if (varDec && nodeIs(varDec, "VarDec")) {
                    char* name = getVarDecName(varDec);
                    if (name) {
                        IRType* baseType = irSpecType(spec);
                        IRType* vtype = irVarDecType(varDec, baseType);
                        char* irName = new_var();
                        int isAddr = irTypeIsAggregate(vtype) ? 1 : 0;
                        irRegVar(name, irName, vtype, isAddr);
                    }
                }
            }
            if (paramDec && paramDec->right && nodeIs(paramDec->right, "COMMA"))
                vl = paramDec->right->right;
            else
                vl = NULL;
        }
        code = concatCodes(code, translate_VarList_Params(afterLp));
    }
    return code;
}

/* ================================================================
 * External definitions - with struct registration
 * ================================================================ */

/*注册结构体在符号表 ，计算内存*/
static void registerStructDefs(ASTNode* node) {
    /* Walk ExtDefList and register named struct definitions */
    ASTNode* dl = node;
    while (dl && nodeIs(dl, "ExtDefList")) {
        ASTNode* extDef = dl->child;
        if (extDef && nodeIs(extDef, "ExtDef")) {
            ASTNode* spec   = extDef->child;
            ASTNode* second = spec ? spec->right : NULL;

            /* Specifier SEMI  => struct-only definition */
            if (nodeIs(second, "SEMI")) {
                ASTNode* specChild = spec ? spec->child : NULL;
                if (nodeIs(specChild, "StructSpecifier")) {
                    ASTNode* ssChild = specChild->child;
                    ASTNode* next = ssChild ? ssChild->right : NULL;
                    /* STRUCT OptTag LC DefList RC */
                    if (next && !nodeIs(next, "Tag")) {
                        ASTNode* lc = next;
                        char* structName = NULL;
                        if (nodeIs(next, "OptTag")) {
                            structName = next->child ? next->child->value : NULL;
                            lc = next->right;
                        }
                        ASTNode* defList = lc ? lc->right : NULL;
                        IRType* st = NULL;
                        if (defList && nodeIs(defList, "DefList")) {
                            st = irBuildStruct(defList);
                        }
                        if (!st) st = irStruct(NULL, 0);
                        if (structName) {
                            irRegStruct(structName, st);
                        }
                    }
                }
            }
        }
        dl = extDef ? extDef->right : NULL;
    }
}

static IRCode* translate_ExtDef(ASTNode* node) {
    if (!node || !nodeIs(node, "ExtDef")) return NULL;
    ASTNode* spec   = node->child;
    ASTNode* second = spec ? spec->right : NULL;

    /* Specifier FunDec CompSt  => function definition 函数定义*/
    if (second && nodeIs(second, "FunDec")) {
        ASTNode* afterFunDec = second->right;
        if (afterFunDec && nodeIs(afterFunDec, "CompSt")) {
            IRCode* code1 = translate_FunDec(second);
            IRCode* code2 = translate_CompSt(afterFunDec);
            return concatCodes(code1, code2);
        }
    }

    /* Specifier ExtDecList SEMI => global var declaration 全局变量声明*/
    if (second && nodeIs(second, "ExtDecList")) {
        IRType* baseType = irSpecType(spec);
        ASTNode* edl = second;
        while (edl && nodeIs(edl, "ExtDecList")) {
            ASTNode* varDec = edl->child;
            char* name = getVarDecName(varDec);
            if (name) {
                IRType* vtype = irVarDecType(varDec, baseType);
                char* irName = new_var();
                irRegVar(name, irName, vtype, 0);
            }
            if (varDec && varDec->right && nodeIs(varDec->right, "COMMA"))
                edl = varDec->right->right;
            else
                edl = NULL;
        }
    }

    return NULL;
}

static IRCode* translate_ExtDefList(ASTNode* node) {
    if (!node || !nodeIs(node, "ExtDefList")) return NULL;

    /* Pass 1: register all struct definitions 注册了所有的结构体，用于计算内存*/
    registerStructDefs(node);

    /* Pass 2: translate functions */
    IRCode* code = NULL;/*要打印的3地址代码*/
    ASTNode* dl = node;
    while (dl && nodeIs(dl, "ExtDefList")) {
        ASTNode* extDef = dl->child;
        if (extDef) {
            IRVar* savedVars = irVars;
            irVars = NULL;
            IRCode* c = translate_ExtDef(extDef);
            code = concatCodes(code, c);/*拼接3地址代码*/
            /* Merge globals from this function back */
            irVars = savedVars;
        }
        dl = extDef ? extDef->right : NULL;
    }
    return code;
}

/* ================================================================
 * Public entry point
 * ================================================================ */

void generateIR(ASTNode* root, const char* outputFile) {
    if (!root) return;

    FILE* out = stdout;
    if (outputFile) {
        out = fopen(outputFile, "w");
        if (!out) {
            fprintf(stderr, "Error opening output file: %s\n", outputFile);
            return;
        }
    }

    ASTNode* extDefList = root->child;
    if (extDefList && nodeIs(extDefList, "ExtDefList")) {
        IRCode* code = translate_ExtDefList(extDefList);
        printIR(code, out);
        freeIR(code);
    }

    if (outputFile && out != stdout) {
        fclose(out);
    }
}
