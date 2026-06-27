#define _POSIX_C_SOURCE 200809L
#include "ir.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

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
 * MIPS32 backend
 * ================================================================ */

typedef struct MIPSVar_ {
    char* name;
    int size;
    int offset;
    int isBlock;
    struct MIPSVar_* next;
} MIPSVar;

typedef struct MIPSFunc_ {
    char* name;
    IRCode* begin;
    IRCode* end;
    MIPSVar* vars;
    char** params;
    int paramCount;
    int frameSize;
    struct MIPSFunc_* next;
} MIPSFunc;

static char* trimSpace(char* s) {
    while (*s && isspace((unsigned char)*s)) s++;
    char* e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) *--e = '\0';
    return s;
}

static int isNameToken(const char* s) {
    if (!s || !*s || s[0] == '#') return 0;
    if (!strcmp(s, "CALL") || !strcmp(s, "GOTO")) return 0;
    if (!isalpha((unsigned char)s[0]) && s[0] != '_') return 0;
    for (const char* p = s + 1; *p; p++) {
        if (!isalnum((unsigned char)*p) && *p != '_') return 0;
    }
    return 1;
}

static MIPSVar* mipsFindVar(MIPSFunc* f, const char* name) {
    for (MIPSVar* v = f->vars; v; v = v->next)
        if (!strcmp(v->name, name)) return v;
    return NULL;
}

static MIPSVar* mipsEnsureVar(MIPSFunc* f, const char* name) {
    if (!isNameToken(name)) return NULL;
    MIPSVar* old = mipsFindVar(f, name);
    if (old) return old;
    MIPSVar* v = (MIPSVar*)malloc(sizeof(MIPSVar));
    v->name = my_strdup(name);
    v->size = 4;
    v->offset = 0;
    v->isBlock = 0;
    v->next = f->vars;
    f->vars = v;
    return v;
}

static void mipsMarkBlock(MIPSFunc* f, const char* name, int size) {
    MIPSVar* v = mipsEnsureVar(f, name);
    if (!v) return;
    v->size = size > 0 ? size : 4;
    v->isBlock = 1;
}

static void mipsAddParam(MIPSFunc* f, const char* name) {
    mipsEnsureVar(f, name);
    f->params = (char**)realloc(f->params, sizeof(char*) * (f->paramCount + 1));
    f->params[f->paramCount++] = my_strdup(name);
}

static void mipsCollectOperand(MIPSFunc* f, const char* op) {
    if (!op || !*op) return;
    if (op[0] == '*') op++;
    if (op[0] == '&') op++;
    mipsEnsureVar(f, op);
}

static MIPSFunc* mipsNewFunc(const char* name, IRCode* begin) {
    MIPSFunc* f = (MIPSFunc*)malloc(sizeof(MIPSFunc));
    f->name = my_strdup(name);
    f->begin = begin;
    f->end = NULL;
    f->vars = NULL;
    f->params = NULL;
    f->paramCount = 0;
    f->frameSize = 0;
    f->next = NULL;
    return f;
}

static MIPSFunc* mipsBuildFuncs(IRCode* ir) {
    MIPSFunc* head = NULL;
    MIPSFunc** tail = &head;
    MIPSFunc* cur = NULL;

    for (IRCode* p = ir; p; p = p->next) {
        char fname[64];
        if (sscanf(p->code, "FUNCTION %63s :", fname) == 1) {
            if (cur) cur->end = p;
            cur = mipsNewFunc(fname, p);
            *tail = cur;
            tail = &cur->next;
            continue;
        }
        if (!cur) continue;

        char a[64], b[64], c[64], d[64], rel[8];
        int sz = 0;
        if (sscanf(p->code, "PARAM %63s", a) == 1) {
            mipsAddParam(cur, a);
        } else if (sscanf(p->code, "DEC %63s %d", a, &sz) == 2) {
            mipsMarkBlock(cur, a, sz);
        } else if (sscanf(p->code, "READ %63s", a) == 1 ||
                   sscanf(p->code, "WRITE %63s", a) == 1 ||
                   sscanf(p->code, "RETURN %63s", a) == 1 ||
                   sscanf(p->code, "ARG %63s", a) == 1) {
            mipsCollectOperand(cur, a);
        } else if (sscanf(p->code, "IF %63s %7s %63s GOTO %63s", a, rel, b, d) == 4) {
            mipsCollectOperand(cur, a);
            mipsCollectOperand(cur, b);
        } else if (sscanf(p->code, "%63s := CALL %63s", a, b) == 2) {
            mipsCollectOperand(cur, a);
        } else if (sscanf(p->code, "*%63s := %63s", a, b) == 2) {
            mipsCollectOperand(cur, a);
            mipsCollectOperand(cur, b);
        } else if (sscanf(p->code, "%63s := %63s %7s %63s", a, b, rel, c) == 4) {
            mipsCollectOperand(cur, a);
            mipsCollectOperand(cur, b);
            mipsCollectOperand(cur, c);
        } else if (sscanf(p->code, "%63s := %63s", a, b) == 2) {
            mipsCollectOperand(cur, a);
            mipsCollectOperand(cur, b);
        }
    }
    if (cur) cur->end = NULL;

    for (MIPSFunc* f = head; f; f = f->next) {
        int off = -12;
        for (MIPSVar* v = f->vars; v; v = v->next) {
            int size = (v->size + 3) / 4 * 4;
            v->offset = off - size + 4;
            off -= size;
        }
        int localBytes = -off - 8;
        f->frameSize = ((localBytes + 8 + 7) / 8) * 8;
        if (f->frameSize < 16) f->frameSize = 16;
    }
    return head;
}

static int mipsVarOffset(MIPSFunc* f, const char* name) {
    MIPSVar* v = mipsFindVar(f, name);
    return v ? v->offset : 0;
}

static void mipsLoadValue(FILE* out, MIPSFunc* f, const char* op, const char* reg) {
    if (!op) return;
    if (op[0] == '#') {
        fprintf(out, "  li %s, %s\n", reg, op + 1);
    } else {
        fprintf(out, "  lw %s, %d($fp)\n", reg, mipsVarOffset(f, op));
    }
}

static void mipsStoreValue(FILE* out, MIPSFunc* f, const char* name, const char* reg) {
    fprintf(out, "  sw %s, %d($fp)\n", reg, mipsVarOffset(f, name));
}

static void mipsLoadAddress(FILE* out, MIPSFunc* f, const char* name, const char* reg) {
    MIPSVar* v = mipsFindVar(f, name);
    if (v && v->isBlock) {
        fprintf(out, "  addiu %s, $fp, %d\n", reg, v->offset);
    } else {
        fprintf(out, "  addiu %s, $fp, %d\n", reg, mipsVarOffset(f, name));
    }
}

static void mipsEmitPrelude(FILE* out) {
    fprintf(out, ".data\n");
    fprintf(out, "_prompt: .asciiz \"Enter an integer:\"\n");
    fprintf(out, "_ret: .asciiz \"\\n\"\n");
    fprintf(out, ".globl main\n");
    fprintf(out, ".text\n");
    fprintf(out, "read:\n");
    fprintf(out, "  li $v0, 4\n");
    fprintf(out, "  la $a0, _prompt\n");
    fprintf(out, "  syscall\n");
    fprintf(out, "  li $v0, 5\n");
    fprintf(out, "  syscall\n");
    fprintf(out, "  jr $ra\n\n");
    fprintf(out, "write:\n");
    fprintf(out, "  li $v0, 1\n");
    fprintf(out, "  syscall\n");
    fprintf(out, "  li $v0, 4\n");
    fprintf(out, "  la $a0, _ret\n");
    fprintf(out, "  syscall\n");
    fprintf(out, "  move $v0, $0\n");
    fprintf(out, "  jr $ra\n\n");
}

static int mipsIsUnsafeName(const char* name) {
    /* MIPS instruction names that conflict with labels in SPIM */
    static const char* unsafe[] = {
        "add", "sub", "mul", "div", "and", "or", "nor", "xor",
        "sll", "srl", "sra", "slt", "sltu",
        "j", "jal", "jr", "jalr",
        "beq", "bne", "bgt", "bge", "blt", "ble",
        "lw", "sw", "lb", "lbu", "sb", "lh", "lhu", "sh",
        "lui", "mfhi", "mflo", "mthi", "mtlo",
        "move", "li", "la", "nop", "syscall",
        "abs", "neg", "negu", "not", "rem", "remu",
        "b", "bczt", "bczf",
        NULL
    };
    for (int i = 0; unsafe[i]; i++)
        if (strcmp(name, unsafe[i]) == 0) return 1;
    return 0;
}

static void mipsEmitFunction(FILE* out, MIPSFunc* f) {
    const char* labelPrefix = mipsIsUnsafeName(f->name) ? "f_" : "";
    fprintf(out, "%s%s:\n", labelPrefix, f->name);
    fprintf(out, "  addiu $sp, $sp, -%d\n", f->frameSize);
    fprintf(out, "  sw $ra, %d($sp)\n", f->frameSize - 4);
    fprintf(out, "  sw $fp, %d($sp)\n", f->frameSize - 8);
    fprintf(out, "  addiu $fp, $sp, %d\n", f->frameSize);

    for (int i = 0; i < f->paramCount; i++) {
        int off = mipsVarOffset(f, f->params[i]);
        if (i < 4) {
            fprintf(out, "  sw $a%d, %d($fp)\n", i, off);
        } else {
            fprintf(out, "  lw $t0, %d($fp)\n", (i - 4) * 4);
            fprintf(out, "  sw $t0, %d($fp)\n", off);
        }
    }

    char endLabel[96];
    snprintf(endLabel, sizeof(endLabel), "%s%s_epilogue", labelPrefix, f->name);
    char* pendingArgs[128];
    int pendingCount = 0;

    for (IRCode* p = f->begin->next; p && p != f->end; p = p->next) {
        char lineBuf[256];
        strncpy(lineBuf, p->code, sizeof(lineBuf) - 1);
        lineBuf[sizeof(lineBuf) - 1] = '\0';
        char* line = trimSpace(lineBuf);
        char a[64], b[64], c[64], lab[64], op[8];
        int sz = 0;

        if (sscanf(line, "PARAM %63s", a) == 1 || sscanf(line, "DEC %63s %d", a, &sz) == 2) {
            continue;
        }
        if (sscanf(line, "LABEL %63s :", lab) == 1) {
            fprintf(out, "%s:\n", lab);
        } else if (sscanf(line, "GOTO %63s", lab) == 1) {
            fprintf(out, "  j %s\n", lab);
        } else if (sscanf(line, "IF %63s %7s %63s GOTO %63s", a, op, b, lab) == 4) {
            mipsLoadValue(out, f, a, "$t0");
            mipsLoadValue(out, f, b, "$t1");
            if (!strcmp(op, "==")) fprintf(out, "  beq $t0, $t1, %s\n", lab);
            else if (!strcmp(op, "!=")) fprintf(out, "  bne $t0, $t1, %s\n", lab);
            else if (!strcmp(op, ">")) fprintf(out, "  bgt $t0, $t1, %s\n", lab);
            else if (!strcmp(op, "<")) fprintf(out, "  blt $t0, $t1, %s\n", lab);
            else if (!strcmp(op, ">=")) fprintf(out, "  bge $t0, $t1, %s\n", lab);
            else if (!strcmp(op, "<=")) fprintf(out, "  ble $t0, $t1, %s\n", lab);
        } else if (sscanf(line, "READ %63s", a) == 1) {
            fprintf(out, "  jal read\n");
            mipsStoreValue(out, f, a, "$v0");
        } else if (sscanf(line, "WRITE %63s", a) == 1) {
            mipsLoadValue(out, f, a, "$a0");
            fprintf(out, "  jal write\n");
        } else if (sscanf(line, "RETURN %63s", a) == 1) {
            mipsLoadValue(out, f, a, "$v0");
            fprintf(out, "  j %s\n", endLabel);
        } else if (sscanf(line, "ARG %63s", a) == 1) {
            if (pendingCount < 128) pendingArgs[pendingCount++] = my_strdup(a);
        } else if (sscanf(line, "%63s := CALL %63s", a, b) == 2 ||
                   sscanf(line, "CALL %63s", b) == 1) {
            for (int i = pendingCount - 1; i >= 4; i--) {
                mipsLoadValue(out, f, pendingArgs[i], "$t0");
                fprintf(out, "  addiu $sp, $sp, -4\n");
                fprintf(out, "  sw $t0, 0($sp)\n");
            }
            for (int i = 0; i < pendingCount && i < 4; i++) {
                mipsLoadValue(out, f, pendingArgs[i], "$t0");
                fprintf(out, "  move $a%d, $t0\n", i);
            }
            fprintf(out, "  jal %s%s\n", mipsIsUnsafeName(b) ? "f_" : "", b);
            if (pendingCount > 4)
                fprintf(out, "  addiu $sp, $sp, %d\n", (pendingCount - 4) * 4);
            for (int i = 0; i < pendingCount; i++) free(pendingArgs[i]);
            pendingCount = 0;
            if (strstr(line, ":= CALL")) mipsStoreValue(out, f, a, "$v0");
        } else if (sscanf(line, "*%63s := %63s", a, b) == 2) {
            mipsLoadValue(out, f, a, "$t0");
            mipsLoadValue(out, f, b, "$t1");
            fprintf(out, "  sw $t1, 0($t0)\n");
        } else if (sscanf(line, "%63s := %63s %7s %63s", a, b, op, c) == 4) {
            mipsLoadValue(out, f, b, "$t0");
            mipsLoadValue(out, f, c, "$t1");
            if (!strcmp(op, "+")) fprintf(out, "  addu $t2, $t0, $t1\n");
            else if (!strcmp(op, "-")) fprintf(out, "  subu $t2, $t0, $t1\n");
            else if (!strcmp(op, "*")) fprintf(out, "  mul $t2, $t0, $t1\n");
            else if (!strcmp(op, "/")) {
                fprintf(out, "  div $t0, $t1\n");
                fprintf(out, "  mflo $t2\n");
            }
            mipsStoreValue(out, f, a, "$t2");
        } else if (sscanf(line, "%63s := *%63s", a, b) == 2) {
            mipsLoadValue(out, f, b, "$t0");
            fprintf(out, "  lw $t1, 0($t0)\n");
            mipsStoreValue(out, f, a, "$t1");
        } else if (sscanf(line, "%63s := &%63s", a, b) == 2) {
            mipsLoadAddress(out, f, b, "$t0");
            mipsStoreValue(out, f, a, "$t0");
        } else if (sscanf(line, "%63s := %63s", a, b) == 2) {
            mipsLoadValue(out, f, b, "$t0");
            mipsStoreValue(out, f, a, "$t0");
        }
    }

    fprintf(out, "%s:\n", endLabel);
    fprintf(out, "  lw $ra, -4($fp)\n");
    fprintf(out, "  lw $fp, -8($fp)\n");
    fprintf(out, "  addiu $sp, $sp, %d\n", f->frameSize);
    fprintf(out, "  jr $ra\n\n");
}

static void generateMIPS(IRCode* ir, FILE* out) {
    MIPSFunc* funcs = mipsBuildFuncs(ir);
    mipsEmitPrelude(out);
    for (MIPSFunc* f = funcs; f; f = f->next) {
        mipsEmitFunction(out, f);
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

    /* Collect all dimension sizes from outermost VarDec to innermost */
    int sizes[16];
    int nDims = 0;
    ASTNode* cur = varDec;
    while (cur && nodeIs(cur, "VarDec")) {
        ASTNode* c = cur->child;
        if (nodeIs(c, "ID")) break;
        ASTNode* lb = c ? c->right : NULL;
        ASTNode* intNode = lb ? lb->right : NULL;
        if (intNode && nodeIs(intNode, "INT")) {
            sizes[nDims++] = atoi(intNode->value);
        }
        cur = c;
    }

    /* Build type from innermost dimension to outermost.
       The VarDec tree stores dimensions with the outermost (root)
       corresponding to the last bracket. E.g., int a[2][3]:
         VarDec(VarDec(ID "a" LB 2 RB) LB 3 RB)
       sizes from root-to-leaf = [3, 2].
       The correct C type is Array(2, Array(3, int)) = int[2][3].
       Building from innermost: int → Array(3, int) → Array(2, Array(3, int)).
       So we apply sizes in root-to-leaf order: [3, 2]. */
    IRType* result = base;
    for (int i = 0; i < nDims; i++) {
        result = irArray(result, sizes[i]);
    }
    return result;
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
 * Expression type resolution — determines the IRType of an Exp node
 * ================================================================ */

static IRType* irExpType(ASTNode* node) {
    if (!node || !nodeIs(node, "Exp")) return irInt();
    ASTNode* first = node->child;
    if (!first) return irInt();

    /* INT literal */
    if (nodeIs(first, "INT")) return irInt();

    /* FLOAT literal */
    if (nodeIs(first, "FLOAT")) return irFloat();

    /* ID */
    if (nodeIs(first, "ID") && !first->right) {
        IRVar* v = irLookupVar(first->value);
        return v ? v->type : irInt();
    }

    /* LP Exp RP */
    if (nodeIs(first, "LP"))
        return irExpType(first->right);

    /* Unary: NOT, MINUS */
    if (nodeIs(first, "NOT") || nodeIs(first, "MINUS"))
        return irInt();

    /* Function call: ID LP ... RP */
    if (nodeIs(first, "ID") && first->right && nodeIs(first->right, "LP"))
        return irInt();

    /* Binary operations */
    if (nodeIs(first, "Exp")) {
        ASTNode* op = first->right;
        ASTNode* rhs = op ? op->right : NULL;
        if (!op) return irInt();

        /* ASSIGNOP, RELOP, AND, OR, PLUS, MINUS, STAR, DIV → all produce int */
        if (nodeIs(op, "ASSIGNOP") || nodeIs(op, "RELOP") ||
            nodeIs(op, "AND") || nodeIs(op, "OR") ||
            nodeIs(op, "PLUS") || nodeIs(op, "MINUS") ||
            nodeIs(op, "STAR") || nodeIs(op, "DIV"))
            return irInt();

        /* DOT: type of the field */
        if (nodeIs(op, "DOT")) {
            IRType* baseType = irExpType(first);
            if (baseType && baseType->kind == IRK_STRUCT && rhs) {
                for (IRField* f = baseType->fields; f; f = f->next)
                    if (strcmp(f->name, rhs->value) == 0) return f->type;
            }
            return irInt();
        }

        /* LB: element type */
        if (nodeIs(op, "LB")) {
            IRType* baseType = irExpType(first);
            if (baseType && baseType->kind == IRK_ARRAY)
                return baseType->elem;
            return irInt();
        }
    }

    return irInt();
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
            IRType* baseType = irExpType(first);
            int offset = irFieldOffset(baseType, rhs->value);

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

            /* Determine element size using type resolution */
            IRType* baseType = irExpType(first);
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
        /* Arrays/structs decay to pointers in rvalue context */
        if (v && irTypeIsAggregate(v->type) && !v->isAddr) {
            return createCode("%s := &%s", place, v->irName);
        }
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

                    /* Check if this is an aggregate assignment (deep copy needed) */
                    IRType* lhsType = irExpType(first);
                    IRType* rhsType = irExpType(rhs);
                    if (lhsType && irTypeIsAggregate(lhsType) &&
                        rhsType && irTypeIsAggregate(rhsType)) {
                        /* Deep copy from RHS address to LHS address */
                        char* srcAddr = NULL;
                        IRCode* code2 = translate_LvalAddr(rhs, &srcAddr);
                        IRCode* code3 = NULL;
                        int copySize = lhsType->size < rhsType->size ? lhsType->size : rhsType->size;
                        for (int off = 0; off < copySize; off += 4) {
                            char* t = new_temp();
                            if (off == 0) {
                                code3 = concatCodes(code3, createCode("%s := *%s", t, srcAddr));
                            } else {
                                char* a = new_temp();
                                code3 = concatCodes(code3, createCode("%s := %s + #%d", a, srcAddr, off));
                                code3 = concatCodes(code3, createCode("%s := *%s", t, a));
                            }
                            if (off == 0) {
                                code3 = concatCodes(code3, createCode("*%s := %s", addr, t));
                            } else {
                                char* a = new_temp();
                                code3 = concatCodes(code3, createCode("%s := %s + #%d", a, addr, off));
                                code3 = concatCodes(code3, createCode("*%s := %s", a, t));
                            }
                        }
                        IRCode* code4 = place ? createCode("%s := %s", place, addr) : NULL;
                        free(addr);
                        free(srcAddr);
                        return concatCodes(concatCodes(code1, code2), concatCodes(code3, code4));
                    }

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

                /* Check for aggregate deep copy (array/struct assignment) */
                IRType* lhsType = irExpType(first);
                IRType* rhsType = irExpType(rhs);
                if (lhsType && irTypeIsAggregate(lhsType) &&
                    rhsType && irTypeIsAggregate(rhsType)) {
                    /* Deep copy: copy words from RHS to LHS */
                    char* destAddr = NULL;
                    IRCode* code1 = translate_LvalAddr(first, &destAddr);
                    char* srcAddr = NULL;
                    IRCode* code2 = translate_LvalAddr(rhs, &srcAddr);
                    IRCode* code3 = NULL;
                    IRCode* code4 = NULL;
                    int copySize = lhsType->size < rhsType->size ? lhsType->size : rhsType->size;
                    for (int off = 0; off < copySize; off += 4) {
                        char* t = new_temp();
                        if (off == 0) {
                            code3 = concatCodes(code3, createCode("%s := *%s", t, srcAddr));
                        } else {
                            char* a = new_temp();
                            code3 = concatCodes(code3, createCode("%s := %s + #%d", a, srcAddr, off));
                            code3 = concatCodes(code3, createCode("%s := *%s", t, a));
                        }
                        if (off == 0) {
                            code4 = concatCodes(code4, createCode("*%s := %s", destAddr, t));
                        } else {
                            char* a = new_temp();
                            code4 = concatCodes(code4, createCode("%s := %s + #%d", a, destAddr, off));
                            code4 = concatCodes(code4, createCode("*%s := %s", a, t));
                        }
                    }
                    IRCode* code5 = place ? createCode("%s := %s", place, destAddr) : NULL;
                    free(destAddr);
                    free(srcAddr);
                    return concatCodes(concatCodes(concatCodes(code1, code2),
                                  concatCodes(code3, code4)), code5);
                }

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
            /* If field is aggregate, return address; otherwise load value */
            IRType* ft = irExpType(node);
            if (irTypeIsAggregate(ft)) {
                IRCode* code2 = createCode("%s := %s", place, addr);
                free(addr);
                return concatCodes(code1, code2);
            }
            IRCode* code2 = createCode("%s := *%s", place, addr);
            free(addr);
            return concatCodes(code1, code2);
        }

        /* --- Array subscript: Exp LB Exp RB (rvalue) --- */
        if (nodeIs(op, "LB")) {
            char* addr = NULL;
            IRCode* code1 = translate_LvalAddr(node, &addr);
            if (!place) return code1;
            /* If element is aggregate, return address; otherwise load value */
            IRType* et = irExpType(node);
            if (irTypeIsAggregate(et)) {
                IRCode* code2 = createCode("%s := %s", place, addr);
                free(addr);
                return concatCodes(code1, code2);
            }
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
            /* Check if argument is a struct/array (aggregate) — pass by address */
            IRType* expType = irExpType(exp);
            int passByAddr = irTypeIsAggregate(expType);
            char* addrName = NULL;

            if (passByAddr) {
                /* For aggregates, get the address instead of loading the value */
                IRCode* addrCode = translate_LvalAddr(exp, &addrName);
                code = concatCodes(code, addrCode);
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
        if (v && irTypeIsAggregate(v->type) && expNode && nodeIs(expNode, "Exp")) {
            /* Aggregate initializer: deep copy from RHS expression */
            IRType* rhsType = irExpType(expNode);
            if (rhsType && irTypeIsAggregate(rhsType)) {
                char* destName = v->irName;
                char* destBase = new_temp();
                IRCode* copyCode = createCode("%s := &%s", destBase, destName);
                char* srcAddr = NULL;
                IRCode* srcCode = translate_LvalAddr(expNode, &srcAddr);
                copyCode = concatCodes(copyCode, srcCode);
                int copySize = v->type->size < rhsType->size ? v->type->size : rhsType->size;
                for (int off = 0; off < copySize; off += 4) {
                    char* t = new_temp();
                    char* saddr = off == 0 ? my_strdup(srcAddr) : NULL;
                    char* daddr = off == 0 ? destBase : NULL;
                    if (off > 0) {
                        saddr = new_temp();
                        copyCode = concatCodes(copyCode, createCode("%s := %s + #%d", saddr, srcAddr, off));
                        daddr = new_temp();
                        copyCode = concatCodes(copyCode, createCode("%s := %s + #%d", daddr, destBase, off));
                    }
                    copyCode = concatCodes(copyCode, createCode("%s := *%s", t, saddr));
                    copyCode = concatCodes(copyCode, createCode("*%s := %s", daddr, t));
                }
                free(srcAddr);
                code = concatCodes(code, copyCode);
            }
        } else if (!v || !irTypeIsAggregate(v->type)) {
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

        /* Detect output format: .ir for IR text, .s for MIPS assembly */
        int isIR = outputFile && strstr(outputFile, ".ir") != NULL;
        if (isIR) {
            printIR(code, out);
        } else {
            generateMIPS(code, out);
        }

        freeIR(code);
    }

    if (outputFile && out != stdout) {
        fclose(out);
    }
}
