#define _POSIX_C_SOURCE 200809L
#include "semantic.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================
 * Type System
 * ================================================================ */

typedef enum {
    T_INT, T_FLOAT, T_ARRAY, T_STRUCT, T_FUNC, T_ERROR
} TypeKind;

/* Forward declare Type_ before FieldList so both can reference each other */
struct Type_;

typedef struct FieldList {
    char*          name;
    struct Type_*  type;
    struct FieldList* next;
} FieldList;

typedef struct Type_ {
    TypeKind kind;
    union {
        struct { struct Type_* elem; int size; } array;
        struct { char* name; FieldList* fields;  } strct;
        struct { struct Type_* ret;  FieldList* params; } func;
    };
} Type;

/* ================================================================
 * Symbol Tables
 * ================================================================ */

typedef struct Symbol {
    char*          name;
    Type*          type;
    int            defined;   /* 1 = var/func-with-body; 0 = func declaration only */
    int            line;      /* line of first declaration (for Error 18) */
    struct Symbol* next;
} Symbol;

static Symbol* varTable    = NULL;   /* variables + functions          */
static Symbol* structDefs  = NULL;   /* named struct type definitions  */
static Type*   curFuncRet  = NULL;   /* return type of current function */

/* ================================================================
 * Type constructors
 * ================================================================ */

static Type* makeInt(void) {
    Type* t = malloc(sizeof(Type));
    t->kind = T_INT;
    return t;
}
static Type* makeFloat(void) {
    Type* t = malloc(sizeof(Type));
    t->kind = T_FLOAT;
    return t;
}
static Type* makeError(void) {
    Type* t = malloc(sizeof(Type));
    t->kind = T_ERROR;
    return t;
}
static Type* makeArray(Type* elem, int size) {
    Type* t = malloc(sizeof(Type));
    t->kind = T_ARRAY;
    t->array.elem = elem;
    t->array.size = size;
    return t;
}
static Type* makeStruct(const char* name, FieldList* fields) {
    Type* t = malloc(sizeof(Type));
    t->kind = T_STRUCT;
    t->strct.name   = name ? my_strdup(name) : NULL;
    t->strct.fields = fields;
    return t;
}
static Type* makeFunc(Type* ret, FieldList* params) {
    Type* t = malloc(sizeof(Type));
    t->kind = T_FUNC;
    t->func.ret    = ret;
    t->func.params = params;
    return t;
}

static Type* makeVoid(void) {
    Type* t = malloc(sizeof(Type));
    t->kind = T_ERROR;
    return t;
}

/* ----------------------------------------------------------------
 * Type equality (name-equivalence for structs; ignore array sizes)
 * ---------------------------------------------------------------- */
static int typeEqual(Type* a, Type* b) {
    if (!a || !b) return 0;
    /* T_ERROR silences cascading errors */
    if (a->kind == T_ERROR || b->kind == T_ERROR) return 1;
    if (a->kind != b->kind) return 0;
    switch (a->kind) {
        case T_INT:
        case T_FLOAT:
            return 1;
        case T_ARRAY:
            return typeEqual(a->array.elem, b->array.elem);
        case T_STRUCT: {
            /* Structural equivalence: same field types in same order */
            FieldList* fa = a->strct.fields;
            FieldList* fb = b->strct.fields;
            while (fa && fb) {
                if (!typeEqual(fa->type, fb->type)) return 0;
                fa = fa->next;
                fb = fb->next;
            }
            return fa == NULL && fb == NULL;
        }
        case T_FUNC: {
            if (!typeEqual(a->func.ret, b->func.ret)) return 0;
            FieldList* pa = a->func.params;
            FieldList* pb = b->func.params;
            while (pa && pb) {
                if (!typeEqual(pa->type, pb->type)) return 0;
                pa = pa->next;
                pb = pb->next;
            }
            return pa == NULL && pb == NULL;
        }
        default:
            return 0;
    }
}

static int isArith(Type* t) {
    return t && (t->kind == T_INT || t->kind == T_FLOAT);
}

/* ================================================================
 * Symbol-table helpers
 * ================================================================ */

static Symbol* lookupVar(const char* name) {
    for (Symbol* s = varTable; s; s = s->next)
        if (strcmp(s->name, name) == 0) return s;
    return NULL;
}
static Symbol* lookupStructDef(const char* name) {
    for (Symbol* s = structDefs; s; s = s->next)
        if (strcmp(s->name, name) == 0) return s;
    return NULL;
}
static void addVarDefined(const char* name, Type* type, int defined, int line) {
    Symbol* s = malloc(sizeof(Symbol));
    s->name    = my_strdup(name);
    s->type    = type;
    s->defined = defined;
    s->line    = line;
    s->next    = varTable;
    varTable   = s;
}
static void addVar(const char* name, Type* type) {
    addVarDefined(name, type, 1, 0);
}
static void addStructDef(const char* name, Type* type) {
    Symbol* s = malloc(sizeof(Symbol));
    s->name = my_strdup(name);
    s->type = type;
    s->next = structDefs;
    structDefs = s;
}

/* ================================================================
 * Error reporting
 * ================================================================ */

static void semError(int errType, int line, const char* msg) {
    printf("Error type %d at Line %d: %s\n", errType, line, msg);
}

/* ================================================================
 * AST helpers
 * ================================================================ */

static int nodeIs(ASTNode* n, const char* t) {
    return n && strcmp(n->type, t) == 0;
}

/* ================================================================
 * Forward declarations
 * ================================================================ */

static Type*      analyzeSpecifier(ASTNode* node);
static Type*      analyzeStructSpecifier(ASTNode* node);
static Type*      analyzeVarDec(ASTNode* node, Type* base, char** nameOut);
static FieldList* buildParamList(ASTNode* vlNode);
static FieldList* buildStructFields(ASTNode* defListNode);
static void       analyzeExtDefList(ASTNode* node);
static void       analyzeExtDef(ASTNode* node);
static void       analyzeDefList(ASTNode* node);
static void       analyzeDef(ASTNode* node);
static void       analyzeCompSt(ASTNode* node);
static void       analyzeStmtList(ASTNode* node);
static void       analyzeStmt(ASTNode* node);
static Type*      analyzeExp(ASTNode* node, int* isLval);
static void       analyzeArgs(ASTNode* argsNode, FieldList* params,
                               int* argCount, int line);

/* ================================================================
 * Specifier → Type
 * ================================================================ */

static Type* analyzeSpecifier(ASTNode* node) {
    if (!node || !nodeIs(node, "Specifier")) return makeError();
    ASTNode* child = node->child;
    if (nodeIs(child, "TYPE")) {
        return (strcmp(child->value, "int") == 0) ? makeInt() : makeFloat();
    }
    if (nodeIs(child, "StructSpecifier")) {
        return analyzeStructSpecifier(child);
    }
    return makeError();
}

static Type* analyzeStructSpecifier(ASTNode* node) {
    if (!node) return makeError();
    /* node->child == STRUCT token */
    ASTNode* next = node->child ? node->child->right : NULL;

    /* ---- STRUCT Tag  (reference) ---- */
    if (nodeIs(next, "Tag")) {
        ASTNode* idNode = next->child;
        const char* name = idNode ? idNode->value : NULL;
        if (!name) return makeError();
        Symbol* s = lookupStructDef(name);
        if (!s) {
            /* Error 17 */
            semError(17, node->line, "Undefined structure name.");
            return makeError();
        }
        return s->type;
    }

    /* ---- STRUCT OptTag LC DefList RC  (definition) ---- */
    char* structName = NULL;
    ASTNode* lc = next;
    if (nodeIs(next, "OptTag")) {
        /* OptTag -> ID */
        ASTNode* idNode = next->child;
        structName = idNode ? idNode->value : NULL;
        lc = next->right;
    }

    /* Error 16: struct name clashes with existing var or struct */
    if (structName) {
        if (lookupVar(structName) || lookupStructDef(structName)) {
            semError(16, node->line, "Duplicated name.");
            structName = NULL; /* don't register a broken type */
        }
    }

    /* Analyse field list */
    ASTNode* defListNode = lc ? lc->right : NULL;
    FieldList* fields = NULL;
    if (defListNode && nodeIs(defListNode, "DefList")) {
        fields = buildStructFields(defListNode);
    }

    /* Unique anonymous name so two anon structs are never equal */
    static int anonCnt = 0;
    char anonBuf[32];
    const char* finalName = structName;
    if (!finalName) {
        snprintf(anonBuf, sizeof(anonBuf), "__anon_%d", anonCnt++);
        finalName = anonBuf;
    }

    Type* t = makeStruct(finalName, fields);
    if (structName)
        addStructDef(structName, t);
    return t;
}

/* ----------------------------------------------------------------
 * Build FieldList for struct body (checks errors 15)
 * ---------------------------------------------------------------- */
static FieldList* buildStructFields(ASTNode* defListNode) {
    FieldList*  head = NULL;
    FieldList** tail = &head;

    /* DefList iteration: DefList->child = Def, Def->right = next DefList */
    ASTNode* dl = defListNode;
    while (dl && nodeIs(dl, "DefList")) {
        ASTNode* def = dl->child;
        if (!def) break;

        /* Def: Specifier DecList SEMI */
        ASTNode* spec    = def->child;
        ASTNode* decList = spec ? spec->right : NULL;
        Type*    base    = analyzeSpecifier(spec);

        /* Iterate DecList: DecList->child = Dec, Dec->right = COMMA DecList */
        ASTNode* dcl = decList;
        while (dcl && nodeIs(dcl, "DecList")) {
            ASTNode* dec    = dcl->child;
            ASTNode* varDec = dec ? dec->child : NULL;

            /* Error 15: field has initialiser */
            if (varDec && varDec->right && nodeIs(varDec->right, "ASSIGNOP")) {
                semError(15, dec->line,
                         "Initialization in struct field definition.");
            }

            char* fname = NULL;
            Type* ftype = analyzeVarDec(varDec, base, &fname);

            if (fname) {
                /* Error 15: duplicate field name */
                int dup = 0;
                for (FieldList* f = head; f; f = f->next)
                    if (strcmp(f->name, fname) == 0) { dup = 1; break; }

                if (dup) {
                    semError(15, dec->line, "Redefined field.");
                } else {
                    FieldList* fl = malloc(sizeof(FieldList));
                    fl->name = my_strdup(fname);
                    fl->type = ftype;
                    fl->next = NULL;
                    *tail = fl;
                    tail  = &fl->next;
                }
            }

            /* advance DecList */
            if (dec && dec->right && nodeIs(dec->right, "COMMA"))
                dcl = dec->right->right;
            else
                dcl = NULL;
        }

        dl = def->right; /* next DefList */
    }
    return head;
}

/* ================================================================
 * VarDec → (Type, name)
 * ================================================================ */

static Type* analyzeVarDec(ASTNode* node, Type* base, char** nameOut) {
    if (!node || !nodeIs(node, "VarDec")) {
        if (nameOut) *nameOut = NULL;
        return makeError();
    }
    ASTNode* child = node->child;

    /* VarDec : ID */
    if (nodeIs(child, "ID")) {
        if (nameOut) *nameOut = child->value;
        return base;
    }

    /* VarDec : VarDec LB INT RB
       child          = inner VarDec
       child->right   = LB
       LB->right      = INT  (size)
       INT->right     = RB   */
    ASTNode* lb      = child ? child->right : NULL;
    ASTNode* intNode = lb    ? lb->right    : NULL;
    int size = (intNode && nodeIs(intNode, "INT")) ? atoi(intNode->value) : 0;

    Type* inner = analyzeVarDec(child, base, nameOut);
    return makeArray(inner, size);
}

/* ================================================================
 * Build parameter FieldList from VarList (no side effects on table)
 * ================================================================ */

static FieldList* buildParamList(ASTNode* vlNode) {
    FieldList*  head = NULL;
    FieldList** tail = &head;

    /* VarList->child = ParamDec, ParamDec->right = COMMA, COMMA->right = VarList */
    ASTNode* vl = vlNode;
    while (vl && nodeIs(vl, "VarList")) {
        ASTNode* paramDec = vl->child;
        ASTNode* spec     = paramDec ? paramDec->child : NULL;
        ASTNode* varDec   = spec     ? spec->right     : NULL;

        Type* base    = analyzeSpecifier(spec);
        char* name    = NULL;
        Type* ptype   = analyzeVarDec(varDec, base, &name);

        if (name) {
            FieldList* fl = malloc(sizeof(FieldList));
            fl->name = my_strdup(name);
            fl->type = ptype;
            fl->next = NULL;
            *tail = fl;
            tail  = &fl->next;
        }

        /* advance VarList */
        if (paramDec && paramDec->right && nodeIs(paramDec->right, "COMMA"))
            vl = paramDec->right->right;
        else
            vl = NULL;
    }
    return head;
}

/* ================================================================
 * ExtDefList / ExtDef  (global scope)
 * ================================================================ */

static void analyzeExtDefList(ASTNode* node) {
    /* ExtDefList->child = ExtDef, ExtDef->right = next ExtDefList */
    ASTNode* dl = node;
    while (dl && nodeIs(dl, "ExtDefList")) {
        ASTNode* extDef = dl->child;
        if (!extDef) break;
        analyzeExtDef(extDef);
        dl = extDef->right;
    }
}

static void analyzeExtDef(ASTNode* node) {
    if (!node || !nodeIs(node, "ExtDef")) return;

    ASTNode* spec   = node->child;
    ASTNode* second = spec ? spec->right : NULL;

    /* ---- Specifier FunDec CompSt  OR  Specifier FunDec SEMI ---- */
    if (nodeIs(second, "FunDec")) {
        Type*    retType = analyzeSpecifier(spec);
        ASTNode* funDec  = second;
        ASTNode* after   = funDec->right;  /* CompSt (definition) or SEMI (declaration) */

        int isDef  = nodeIs(after, "CompSt");

        /* funDec: ID LP [VarList] RP */
        ASTNode* funcId  = funDec->child;
        ASTNode* lp      = funcId ? funcId->right  : NULL;
        ASTNode* afterLp = lp     ? lp->right       : NULL;
        char*    fname   = funcId ? funcId->value    : NULL;

        if (!fname) return;

        /* Build param list (no side effects on symbol table) */
        FieldList* params = NULL;
        if (afterLp && nodeIs(afterLp, "VarList"))
            params = buildParamList(afterLp);

        Type* funcType = makeFunc(retType, params);

        Symbol* existing = lookupVar(fname);
        if (existing) {
            if (existing->type->kind != T_FUNC) {
                /* 名字已被变量占用 */
                semError(3, funDec->line, "Redefined variable.");
            } else if (existing->defined && isDef) {
                /* 两次都有函数体 —— 真正的重复定义 */
                semError(4, funDec->line, "Redefined function.");
            } else {
                /* 至少一方是纯声明：检查签名是否一致 */
                if (!typeEqual(existing->type, funcType)) {
                    /* 返回值类型或参数列表不匹配 */
                    semError(19, funDec->line,
                             "Conflict between function declarations or between declaration and definition.");
                } else if (isDef) {
                    /* 签名一致，且本次是定义体 —— 将声明升级为已定义 */
                    existing->defined = 1;
                }
                /* 签名一致、再次纯声明：静默忽略 */
            }
        } else {
            addVarDefined(fname, funcType, isDef ? 1 : 0, funDec->line);
        }

        if (isDef) {
            /* Function scope: save, add params, analyse body, restore */
            Symbol* savedScope = varTable;
            for (FieldList* p = params; p; p = p->next) {
                if (lookupVar(p->name) || lookupStructDef(p->name))
                    semError(3, funDec->line, "Redefined variable.");
                else
                    addVar(p->name, p->type);
            }
            Type* prevRet = curFuncRet;
            curFuncRet = retType;
            analyzeCompSt(after);
            curFuncRet = prevRet;
            varTable = savedScope;
        }
        return;
    }

    /* ---- Specifier ExtDecList SEMI  (global var declaration) ---- */
    if (nodeIs(second, "ExtDecList")) {
        Type* base = analyzeSpecifier(spec);

        /* ExtDecList->child = VarDec, VarDec->right = COMMA, COMMA->right = ExtDecList */
        ASTNode* edl = second;
        while (edl && nodeIs(edl, "ExtDecList")) {
            ASTNode* varDec = edl->child;
            char*    name   = NULL;
            Type*    vtype  = analyzeVarDec(varDec, base, &name);

            if (name) {
                if (lookupVar(name) || lookupStructDef(name))
                    /* Error 3 */
                    semError(3, varDec->line, "Redefined variable.");
                else
                    addVar(name, vtype);
            }

            /* advance */
            if (varDec && varDec->right && nodeIs(varDec->right, "COMMA"))
                edl = varDec->right->right;
            else
                edl = NULL;
        }
        return;
    }

    /* ---- Specifier SEMI  (struct-only definition) ---- */
    if (nodeIs(second, "SEMI")) {
        analyzeSpecifier(spec); /* side-effect: registers struct */
    }
}

/* ================================================================
 * DefList / Def  (local variable declarations)
 * ================================================================ */

static void analyzeDefList(ASTNode* node) {
    /* DefList->child = Def, Def->right = next DefList */
    ASTNode* dl = node;
    while (dl && nodeIs(dl, "DefList")) {
        ASTNode* def = dl->child;
        if (!def) break;
        analyzeDef(def);
        dl = def->right;
    }
}

static void analyzeDef(ASTNode* node) {
    if (!node || !nodeIs(node, "Def")) return;

    /* Def: Specifier DecList SEMI */
    ASTNode* spec    = node->child;
    ASTNode* decList = spec ? spec->right : NULL;
    Type*    base    = analyzeSpecifier(spec);

    /* DecList->child = Dec, Dec->right = COMMA DecList */
    ASTNode* dcl = decList;
    while (dcl && nodeIs(dcl, "DecList")) {
        ASTNode* dec    = dcl->child;
        ASTNode* varDec = dec ? dec->child : NULL;

        char* name  = NULL;
        Type* vtype = analyzeVarDec(varDec, base, &name);

        if (name) {
            if (lookupVar(name) || lookupStructDef(name))
                semError(3, dec->line, "Redefined variable.");  /* Error 3 */
            else
                addVar(name, vtype);
        }

        /* Optional initializer: VarDec ASSIGNOP Exp */
        ASTNode* assignop = varDec ? varDec->right : NULL;
        if (assignop && nodeIs(assignop, "ASSIGNOP")) {
            ASTNode* expNode = assignop->right;
            int lv = 0;
            Type* rhs = analyzeExp(expNode, &lv);
            if (!typeEqual(vtype, rhs))
                semError(5, dec->line,
                         "Type mismatched for assignment.");    /* Error 5 */
        }

        /* advance DecList */
        if (dec && dec->right && nodeIs(dec->right, "COMMA"))
            dcl = dec->right->right;
        else
            dcl = NULL;
    }
}

/* ================================================================
 * CompSt
 * ================================================================ */

static void analyzeCompSt(ASTNode* node) {
    if (!node || !nodeIs(node, "CompSt")) return;
    /* CompSt: LC [DefList] [StmtList] RC
       child = LC
       LC->right = DefList | StmtList | RC   */
    ASTNode* lc  = node->child;
    ASTNode* cur = lc ? lc->right : NULL;

    if (cur && nodeIs(cur, "DefList")) {
        analyzeDefList(cur);
        cur = cur->right;   /* StmtList or RC */
    }
    if (cur && nodeIs(cur, "StmtList")) {
        analyzeStmtList(cur);
    }
}

/* ================================================================
 * StmtList / Stmt
 * ================================================================ */

static void analyzeStmtList(ASTNode* node) {
    /* StmtList->child = Stmt, Stmt->right = next StmtList */
    ASTNode* sl = node;
    while (sl && nodeIs(sl, "StmtList")) {
        ASTNode* stmt = sl->child;
        if (!stmt) break;
        analyzeStmt(stmt);
        sl = stmt->right;
    }
}

static void analyzeStmt(ASTNode* node) {
    if (!node || !nodeIs(node, "Stmt")) return;
    ASTNode* first = node->child;
    if (!first) return;

    /* Exp SEMI */
    if (nodeIs(first, "Exp")) {
        int lv = 0;
        analyzeExp(first, &lv);
        return;
    }

    /* CompSt */
    if (nodeIs(first, "CompSt")) {
        analyzeCompSt(first);
        return;
    }

    /* RETURN Exp SEMI */
    if (nodeIs(first, "RETURN")) {
        ASTNode* expNode = first->right;
        int lv = 0;
        Type* ret = analyzeExp(expNode, &lv);
        if (curFuncRet && !typeEqual(ret, curFuncRet))
            semError(8, node->line,
                     "Type mismatched for return.");            /* Error 8 */
        return;
    }

    /* IF LP Exp RP Stmt [ELSE Stmt]
       first = IF, IF->right = LP, LP->right = Exp,
       Exp->right = RP, RP->right = Stmt1, [Stmt1->right = ELSE, ELSE->right = Stmt2] */
    if (nodeIs(first, "IF")) {
        ASTNode* lp    = first->right;
        ASTNode* exp   = lp   ? lp->right  : NULL;
        ASTNode* rp    = exp  ? exp->right  : NULL;
        ASTNode* stmt1 = rp   ? rp->right   : NULL;

        int lv = 0;
        Type* cond = analyzeExp(exp, &lv);
        if (cond && cond->kind != T_INT && cond->kind != T_ERROR)
            semError(7, node->line,
                     "Condition expression must be of integer type.");

        analyzeStmt(stmt1);

        if (stmt1 && stmt1->right && nodeIs(stmt1->right, "ELSE"))
            analyzeStmt(stmt1->right->right);
        return;
    }

    /* WHILE LP Exp RP Stmt */
    if (nodeIs(first, "WHILE")) {
        ASTNode* lp   = first->right;
        ASTNode* exp  = lp  ? lp->right  : NULL;
        ASTNode* rp   = exp ? exp->right  : NULL;
        ASTNode* body = rp  ? rp->right   : NULL;

        int lv = 0;
        Type* cond = analyzeExp(exp, &lv);
        if (cond && cond->kind != T_INT && cond->kind != T_ERROR)
            semError(7, node->line,
                     "Condition expression must be of integer type.");

        analyzeStmt(body);
        return;
    }
}

/* ================================================================
 * Argument checking helper
 * ================================================================ */

static void analyzeArgs(ASTNode* argsNode, FieldList* params,
                         int* argCount, int line) {
    /* Args->child = Exp, Exp->right = COMMA, COMMA->right = Args */
    ASTNode* cur    = argsNode;
    FieldList* param = params;

    while (cur && nodeIs(cur, "Args")) {
        ASTNode* expNode = cur->child;
        int lv = 0;
        Type* argType = analyzeExp(expNode, &lv);

        if (param) {
            if (!typeEqual(argType, param->type))
                semError(9, line,
                         "Function arguments type mismatch.");  /* Error 9 */
            param = param->next;
        }
        (*argCount)++;

        /* advance */
        if (expNode && expNode->right && nodeIs(expNode->right, "COMMA"))
            cur = expNode->right->right;
        else
            cur = NULL;
    }
}

/* ================================================================
 * Exp → (Type, isLval)
 * ================================================================ */

static Type* analyzeExp(ASTNode* node, int* isLval) {
    if (isLval) *isLval = 0;
    if (!node || !nodeIs(node, "Exp")) return makeError();

    ASTNode* first = node->child;
    if (!first) return makeError();

    /* ---- INT literal ---- */
    if (nodeIs(first, "INT"))   return makeInt();

    /* ---- FLOAT literal ---- */
    if (nodeIs(first, "FLOAT")) return makeFloat();

    /* ---- LP Exp RP ---- */
    if (nodeIs(first, "LP")) {
        return analyzeExp(first->right, isLval);
    }

    /* ---- Unary MINUS: child = MINUS, MINUS->right = Exp ---- */
    if (nodeIs(first, "MINUS") && first->right && nodeIs(first->right, "Exp")) {
        int lv = 0;
        Type* t = analyzeExp(first->right, &lv);
        if (!isArith(t) && t->kind != T_ERROR)
            semError(7, node->line, "Operand type mismatch.");  /* Error 7 */
        return (t->kind != T_ERROR) ? t : makeError();
    }

    /* ---- Unary NOT: child = NOT, NOT->right = Exp ---- */
    if (nodeIs(first, "NOT")) {
        int lv = 0;
        Type* t = analyzeExp(first->right, &lv);
        if (t->kind != T_INT && t->kind != T_ERROR)
            semError(7, node->line, "Operand must be integer for NOT.");
        return makeInt();
    }

    /* ---- ID (plain variable) ---- */
    if (nodeIs(first, "ID") && !first->right) {
        Symbol* sym = lookupVar(first->value);
        if (!sym) {
            semError(1, node->line, "Undefined variable."); /* Error 1 */
            return makeError();
        }
        /* A function name alone is not an lvalue */
        if (sym->type->kind != T_FUNC && isLval) *isLval = 1;
        return sym->type;
    }

    /* ---- Function call: ID LP ... RP ---- */
    if (nodeIs(first, "ID") && first->right && nodeIs(first->right, "LP")) {
        char*    fname   = first->value;
        ASTNode* lp      = first->right;
        ASTNode* afterLp = lp->right;        /* Args or RP */

        Symbol* sym = lookupVar(fname);
        if (!sym) {
            semError(2, node->line, "Undefined function.");   /* Error 2 */
            return makeError();
        }
        if (sym->type->kind != T_FUNC) {
            semError(11, node->line,
                     "\"(...)\" applied to non-function.");    /* Error 11 */
            return makeError();
        }

        FieldList* params     = sym->type->func.params;
        int        paramCount = 0;
        for (FieldList* p = params; p; p = p->next) paramCount++;
        int argCount = 0;

        if (nodeIs(afterLp, "Args")) {
            analyzeArgs(afterLp, params, &argCount, node->line);
        }
        /* argCount == 0 when ID LP RP */

        if (argCount != paramCount)
            semError(9, node->line,
                     "Function arguments count mismatch.");    /* Error 9 */

        return sym->type->func.ret;
    }

    /* ---- Binary expressions: first child is Exp ---- */
    if (nodeIs(first, "Exp")) {
        ASTNode* op  = first->right;
        ASTNode* rhs = op ? op->right : NULL;

        if (!op) return makeError();

        /* --- ASSIGNOP --- */
        if (nodeIs(op, "ASSIGNOP")) {
            int lv1 = 0, lv2 = 0;
            Type* t1 = analyzeExp(first, &lv1);
            Type* t2 = analyzeExp(rhs,   &lv2);

            if (!lv1)
                semError(6, node->line,
                         "The left-hand side of an assignment must be a variable.");
            if (!typeEqual(t1, t2))
                semError(5, node->line,
                         "Type mismatched for assignment.");   /* Error 5 */
            return t1;
        }

        /* --- Array subscript: Exp LB Exp RB --- */
        if (nodeIs(op, "LB")) {
            int lv1 = 0, lv2 = 0;
            Type* arr = analyzeExp(first, &lv1);
            /* rhs is the index Exp; rhs->right == RB */
            Type* idx = analyzeExp(rhs, &lv2);

            if (idx && idx->kind != T_INT && idx->kind != T_ERROR)
                semError(12, node->line,
                         "Array subscript is not an integer."); /* Error 12 */

            if (!arr || (arr->kind != T_ARRAY && arr->kind != T_ERROR)) {
                semError(10, node->line,
                         "Subscripting non-array variable.");   /* Error 10 */
                return makeError();
            }
            if (isLval) *isLval = 1;
            return (arr->kind == T_ARRAY) ? arr->array.elem : makeError();
        }

        /* --- Struct field access: Exp DOT ID --- */
        if (nodeIs(op, "DOT")) {
            /* rhs is an ID node (not Exp) */
            int lv1 = 0;
            Type* st = analyzeExp(first, &lv1);

            if (!st || (st->kind != T_STRUCT && st->kind != T_ERROR)) {
                semError(13, node->line,
                         "Accessing fields of non-struct variable."); /* Error 13 */
                return makeError();
            }
            if (st->kind == T_STRUCT && rhs) {
                char* fname = rhs->value;
                for (FieldList* f = st->strct.fields; f; f = f->next) {
                    if (strcmp(f->name, fname) == 0) {
                        if (isLval) *isLval = 1;
                        return f->type;
                    }
                }
                semError(14, node->line,
                         "Non-existent field.");               /* Error 14 */
                return makeError();
            }
            return makeError();
        }

        /* --- Logical AND / OR (int only) --- */
        if (nodeIs(op, "AND") || nodeIs(op, "OR")) {
            int lv1 = 0, lv2 = 0;
            Type* t1 = analyzeExp(first, &lv1);
            Type* t2 = analyzeExp(rhs,   &lv2);
            if ((t1->kind != T_INT && t1->kind != T_ERROR) ||
                (t2->kind != T_INT && t2->kind != T_ERROR))
                semError(7, node->line,
                         "Operands of && / || must be integers."); /* Error 7 */
            return makeInt();
        }

        /* --- RELOP (int or float, same type) --- */
        if (nodeIs(op, "RELOP")) {
            int lv1 = 0, lv2 = 0;
            Type* t1 = analyzeExp(first, &lv1);
            Type* t2 = analyzeExp(rhs,   &lv2);
            if (t1->kind != T_ERROR && t2->kind != T_ERROR) {
                if (!isArith(t1) || !isArith(t2) || !typeEqual(t1, t2))
                    semError(7, node->line, "Operand type mismatch."); /* Error 7 */
            }
            return makeInt();
        }

        /* --- Arithmetic: PLUS MINUS STAR DIV (int or float, same type) --- */
        if (nodeIs(op, "PLUS")  || nodeIs(op, "MINUS") ||
            nodeIs(op, "STAR")  || nodeIs(op, "DIV")) {
            int lv1 = 0, lv2 = 0;
            Type* t1 = analyzeExp(first, &lv1);
            Type* t2 = analyzeExp(rhs,   &lv2);
            if (t1->kind != T_ERROR && t2->kind != T_ERROR) {
                if (!isArith(t1) || !isArith(t2)) {
                    semError(7, node->line, "Operand type mismatch."); /* Error 7 */
                    return makeError();
                }
                if (!typeEqual(t1, t2)) {
                    semError(7, node->line, "Operand type mismatch."); /* Error 7 */
                    return makeError();
                }
            }
            return (t1->kind != T_ERROR) ? t1 : t2;
        }
    }

    return makeError();
}

/* ================================================================
 * Public entry point
 * ================================================================ */

void print_ast_token(ASTNode* root) {
    return;
}



void semanticAnalysis(ASTNode* root) {
    if (!root) return;
    /* Reset global state (supports multiple calls in unit tests) */
    varTable   = NULL;
    structDefs = NULL;
    curFuncRet = NULL;

    /* add READ and WRITE function for task3*/
    Type* READ_ret = makeInt();
    Type* READ_function = makeFunc(READ_ret, NULL);
    addVarDefined("read", READ_function, 1, 0);

    FieldList* param = malloc(sizeof(FieldList));
    param->name = my_strdup("x");
    param->type = makeInt();
    param->next = NULL;

    Type* WRITE_ret = makeVoid();
    Type* WRITE_function = makeFunc(WRITE_ret, param);
    addVarDefined("write", WRITE_function, 1, 0);
    /*------------------------------
    1.void return
    2.main function undeclared
    3.read function and write function
    ------------------------------*/

    /* root: Program -> child = ExtDefList */
    ASTNode* extDefList = root->child;
    if (extDefList && nodeIs(extDefList, "ExtDefList"))
        analyzeExtDefList(extDefList);

    /* Error 18: functions declared but never defined */
    for (Symbol* s = varTable; s; s = s->next) {
        if (s->type && s->type->kind == T_FUNC && !s->defined)
            semError(18, s->line, "Function declared but not defined.");
    }
}
