#define _POSIX_C_SOURCE 200809L
#include "optimize.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

/* ================================================================
 * IR string utility helpers
 * ================================================================ */

static char* my_strdup(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1;
    char* d = malloc(len);
    if (d) memcpy(d, s, len);
    return d;
}

static char* trim(char* s) {
    while (*s && isspace((unsigned char)*s)) s++;
    char* e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) *--e = '\0';
    return s;
}

/* Does a string look like a variable name (not a constant)? */
static int isVar(const char* s) {
    if (!s || !*s) return 0;
    if (s[0] == '#') return 0;  /* constant */
    return 1;
}

/* Is a string a numeric constant (starts with #)? */
static int isConst(const char* s) {
    return s && s[0] == '#';
}

/* Extract constant value from "#N" string */
static int constVal(const char* s) {
    return s && s[0] == '#' ? atoi(s + 1) : 0;
}

/* ================================================================
 * Instruction classification
 * ================================================================ */

typedef enum {
    I_NONE, I_FUNCTION, I_PARAM, I_DEC, I_LABEL,
    I_GOTO, I_IF, I_READ, I_WRITE, I_RETURN, I_ARG,
    I_ASSIGN,       /* x := y op z */
    I_COPY,         /* x := y (y is variable or constant) */
    I_STORE,        /* *x := y */
    I_LOAD,         /* x := *y */
    I_ADDR,         /* x := &y */
    I_CALL,         /* x := CALL f  or  CALL f */
} IKind;

typedef struct {
    IKind kind;
    char* line;    /* original line string */

    char dest[64];
    char src1[64];
    char src2[64];
    char op[8];
    char label[64];

    /* for copy propagation: is dest just a copy of src1? */
    int isCopy;
    char copySrc[64];

    /* for constant propagation */
    int hasConstVal;
    int constVal;

    struct BasicBlock* bb;  /* owning basic block */
} Inst;

/* ================================================================
 * Basic Block
 * ================================================================ */

typedef struct BasicBlock {
    int id;
    char label[64];      /* label at entry, if any */
    Inst** insts;
    int count;
    int cap;

    struct BasicBlock** preds;
    int predCount;
    struct BasicBlock** succs;
    int succCount;

    int visited;
    int inLoop;
} BasicBlock;

/* ================================================================
 * Parse an IR line into a structured Inst
 * ================================================================ */

static Inst* instNew(const char* line) {
    Inst* inst = calloc(1, sizeof(Inst));
    inst->line = my_strdup(line);
    return inst;
}

static void instFree(Inst* i) {
    if (!i) return;
    free(i->line);
    free(i);
}

static void parseInst(const char* line, Inst* inst) {
    char buf[256];
    strncpy(buf, line, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    char* s = trim(buf);
    if (!s || !*s) { inst->kind = I_NONE; return; }

    inst->kind = I_NONE;

    /* FUNCTION name : */
    {
        char a[64];
        if (sscanf(s, "FUNCTION %63s :", a) == 1) {
            inst->kind = I_FUNCTION;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            return;
        }
    }

    /* PARAM name */
    {
        char a[64];
        if (sscanf(s, "PARAM %63s", a) == 1) {
            inst->kind = I_PARAM;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            return;
        }
    }

    /* DEC name size */
    {
        char a[64]; int sz;
        if (sscanf(s, "DEC %63s %d", a, &sz) == 2) {
            inst->kind = I_DEC;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            inst->constVal = sz;
            return;
        }
    }

    /* LABEL name : */
    {
        char a[64];
        if (sscanf(s, "LABEL %63s :", a) == 1) {
            inst->kind = I_LABEL;
            snprintf(inst->label, sizeof(inst->label), "%s", a);
            return;
        }
    }

    /* IF src1 RELOP src2 GOTO label */
    {
        char a[64], op[8], b[64], lab[64];
        if (sscanf(s, "IF %63s %7s %63s GOTO %63s", a, op, b, lab) == 4) {
            inst->kind = I_IF;
            snprintf(inst->src1, sizeof(inst->src1), "%s", a);
            snprintf(inst->op, sizeof(inst->op), "%s", op);
            snprintf(inst->src2, sizeof(inst->src2), "%s", b);
            snprintf(inst->label, sizeof(inst->label), "%s", lab);
            inst->hasConstVal = (isConst(a) && isConst(b));
            if (inst->hasConstVal) {
                int v1 = constVal(a), v2 = constVal(b);
                int result = 0;
                if (!strcmp(op, "==")) result = (v1 == v2);
                else if (!strcmp(op, "!=")) result = (v1 != v2);
                else if (!strcmp(op, ">")) result = (v1 > v2);
                else if (!strcmp(op, "<")) result = (v1 < v2);
                else if (!strcmp(op, ">=")) result = (v1 >= v2);
                else if (!strcmp(op, "<=")) result = (v1 <= v2);
                inst->constVal = result;
            }
            return;
        }
    }

    /* GOTO label */
    {
        char a[64];
        if (sscanf(s, "GOTO %63s", a) == 1) {
            inst->kind = I_GOTO;
            snprintf(inst->label, sizeof(inst->label), "%s", a);
            return;
        }
    }

    /* READ x */
    {
        char a[64];
        if (sscanf(s, "READ %63s", a) == 1) {
            inst->kind = I_READ;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            return;
        }
    }

    /* WRITE x */
    {
        char a[64];
        if (sscanf(s, "WRITE %63s", a) == 1) {
            inst->kind = I_WRITE;
            snprintf(inst->src1, sizeof(inst->src1), "%s", a);
            return;
        }
    }

    /* RETURN x */
    {
        char a[64];
        if (sscanf(s, "RETURN %63s", a) == 1) {
            inst->kind = I_RETURN;
            snprintf(inst->src1, sizeof(inst->src1), "%s", a);
            return;
        }
    }

    /* ARG x */
    {
        char a[64];
        if (sscanf(s, "ARG %63s", a) == 1) {
            inst->kind = I_ARG;
            snprintf(inst->src1, sizeof(inst->src1), "%s", a);
            return;
        }
    }

    /* *x := y  (store) */
    {
        char a[64], b[64];
        if (sscanf(s, "*%63[^ ] := %63s", a, b) == 2) {
            inst->kind = I_STORE;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            snprintf(inst->src1, sizeof(inst->src1), "%s", b);
            return;
        }
    }

    /* x := *y (load) */
    {
        char a[64], b[64];
        if (sscanf(s, "%63[^ ] := *%63s", a, b) == 2) {
            inst->kind = I_LOAD;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            snprintf(inst->src1, sizeof(inst->src1), "%s", b);
            return;
        }
    }

    /* x := &y (address-of) */
    {
        char a[64], b[64];
        if (sscanf(s, "%63[^ ] := &%63s", a, b) == 2) {
            inst->kind = I_ADDR;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            snprintf(inst->src1, sizeof(inst->src1), "%s", b);
            return;
        }
    }

    /* x := CALL f */
    {
        char a[64], b[64];
        if (sscanf(s, "%63[^ ] := CALL %63s", a, b) == 2) {
            inst->kind = I_CALL;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            snprintf(inst->src1, sizeof(inst->src1), "%s", b);
            return;
        }
    }

    /* CALL f */
    {
        char a[64];
        if (sscanf(s, "CALL %63s", a) == 1) {
            inst->kind = I_CALL;
            snprintf(inst->src1, sizeof(inst->src1), "%s", a);
            return;
        }
    }

    /* x := y op z  (binary operation) */
    {
        char a[64], b[64], op[8], c[64];
        if (sscanf(s, "%63[^ ] := %63s %7s %63s", a, b, op, c) == 4) {
            inst->kind = I_ASSIGN;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            snprintf(inst->src1, sizeof(inst->src1), "%s", b);
            snprintf(inst->op, sizeof(inst->op), "%s", op);
            snprintf(inst->src2, sizeof(inst->src2), "%s", c);

            /* Check for constant folding */
            if (isConst(b) && isConst(c)) {
                inst->hasConstVal = 1;
                int v1 = constVal(b), v2 = constVal(c);
                if (!strcmp(op, "+")) inst->constVal = v1 + v2;
                else if (!strcmp(op, "-")) inst->constVal = v1 - v2;
                else if (!strcmp(op, "*")) inst->constVal = v1 * v2;
                else if (!strcmp(op, "/")) {
                    inst->constVal = (v2 != 0) ? v1 / v2 : 0;
                }
            }

            /* Check for copy propagation patterns: x := y + 0, x := y - 0, x := y * 1 */
            if (isConst(c)) {
                int v2 = constVal(c);
                if ((!strcmp(op, "+") || !strcmp(op, "-")) && v2 == 0 && isVar(b)) {
                    inst->isCopy = 1;
                    snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", b);
                }
                if (!strcmp(op, "*") && v2 == 1 && isVar(b)) {
                    inst->isCopy = 1;
                    snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", b);
                }
            }
            if (isConst(b)) {
                int v1 = constVal(b);
                if (!strcmp(op, "+") && v1 == 0 && isVar(c)) {
                    inst->isCopy = 1;
                    snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", c);
                }
            }

            return;
        }
    }

    /* x := y  (simple copy) */
    {
        char a[64], b[64];
        if (sscanf(s, "%63[^ ] := %63s", a, b) == 2) {
            inst->kind = I_COPY;
            snprintf(inst->dest, sizeof(inst->dest), "%s", a);
            snprintf(inst->src1, sizeof(inst->src1), "%s", b);
            inst->isCopy = 1;
            snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", b);
            if (isConst(b)) {
                inst->hasConstVal = 1;
                inst->constVal = constVal(b);
            }
            return;
        }
    }
}

static int isTerminator(IKind k) {
    return k == I_GOTO || k == I_RETURN || k == I_IF;
}

/* ================================================================
 * Build basic blocks from IRCode linked list
 * ================================================================ */

static BasicBlock* bbNew(int id, const char* label) {
    BasicBlock* bb = calloc(1, sizeof(BasicBlock));
    bb->id = id;
    bb->cap = 16;
    bb->insts = malloc(sizeof(Inst*) * bb->cap);
    if (label) snprintf(bb->label, sizeof(bb->label), "%s", label);
    return bb;
}

static void bbAddInst(BasicBlock* bb, Inst* inst) {
    if (bb->count >= bb->cap) {
        bb->cap *= 2;
        bb->insts = realloc(bb->insts, sizeof(Inst*) * bb->cap);
    }
    bb->insts[bb->count++] = inst;
    inst->bb = bb;
}

static void bbFree(BasicBlock* bb) {
    if (!bb) return;
    for (int i = 0; i < bb->count; i++) instFree(bb->insts[i]);
    free(bb->insts);
    free(bb->preds);
    free(bb->succs);
    free(bb);
}

typedef struct {
    BasicBlock** blocks;
    int count;
    int cap;
} CFG;

static CFG* cfgNew(void) {
    CFG* cfg = calloc(1, sizeof(CFG));
    cfg->cap = 32;
    cfg->blocks = malloc(sizeof(BasicBlock*) * cfg->cap);
    return cfg;
}

static void cfgAddBlock(CFG* cfg, BasicBlock* bb) {
    if (cfg->count >= cfg->cap) {
        cfg->cap *= 2;
        cfg->blocks = realloc(cfg->blocks, sizeof(BasicBlock*) * cfg->cap);
    }
    cfg->blocks[cfg->count++] = bb;
}

static BasicBlock* cfgFindLabel(CFG* cfg, const char* label) {
    if (!label || !*label) return NULL;
    for (int i = 0; i < cfg->count; i++) {
        if (cfg->blocks[i]->label[0] &&
            !strcmp(cfg->blocks[i]->label, label))
            return cfg->blocks[i];
    }
    return NULL;
}

static void bbAddPred(BasicBlock* bb, BasicBlock* pred) {
    for (int i = 0; i < bb->predCount; i++)
        if (bb->preds[i] == pred) return;
    bb->preds = realloc(bb->preds, sizeof(BasicBlock*) * (bb->predCount + 1));
    bb->preds[bb->predCount++] = pred;
}

static void bbAddSucc(BasicBlock* bb, BasicBlock* succ) {
    for (int i = 0; i < bb->succCount; i++)
        if (bb->succs[i] == succ) return;
    bb->succs = realloc(bb->succs, sizeof(BasicBlock*) * (bb->succCount + 1));
    bb->succs[bb->succCount++] = succ;
}

static void cfgFree(CFG* cfg) {
    if (!cfg) return;
    for (int i = 0; i < cfg->count; i++) bbFree(cfg->blocks[i]);
    free(cfg->blocks);
    free(cfg);
}

/* Build CFG: split IR into functions, for each function build basic blocks */
static CFG* buildCFG(IRCode* ir) {
    CFG* cfg = cfgNew();
    BasicBlock* curBB = NULL;
    char pendingLabel[64] = "";
    Inst* prevInst = NULL;

    for (IRCode* p = ir; p; p = p->next) {
        Inst* inst = instNew(p->code);
        parseInst(p->code, inst);

        if (inst->kind == I_FUNCTION) {
            /* New function: start new basic block */
            curBB = bbNew(cfg->count, "");
            cfgAddBlock(cfg, curBB);
            bbAddInst(curBB, inst);
            prevInst = inst;
            pendingLabel[0] = '\0';
            continue;
        }

        if (inst->kind == I_LABEL) {
            /* Check if label is already a pending entry for a new block */
            snprintf(pendingLabel, sizeof(pendingLabel), "%s", inst->label);
            /* If we're in a block that already has instructions, start new block */
            if (curBB && curBB->count > 0) {
                curBB = bbNew(cfg->count, pendingLabel);
                cfgAddBlock(cfg, curBB);
            } else if (!curBB) {
                curBB = bbNew(cfg->count, pendingLabel);
                cfgAddBlock(cfg, curBB);
            } else {
                snprintf(curBB->label, sizeof(curBB->label), "%s", inst->label);
            }
            bbAddInst(curBB, inst);
            prevInst = inst;
            pendingLabel[0] = '\0';
            continue;
        }

        /* If we need to start a new block (first instruction, or prev was terminator) */
        if (!curBB || (prevInst && isTerminator(prevInst->kind))) {
            curBB = bbNew(cfg->count, pendingLabel);
            cfgAddBlock(cfg, curBB);
            pendingLabel[0] = '\0';
        }

        if (!curBB) {
            curBB = bbNew(cfg->count, "");
            cfgAddBlock(cfg, curBB);
        }

        bbAddInst(curBB, inst);
        prevInst = inst;

        if (isTerminator(inst->kind)) {
            curBB = NULL;
        }
    }

    return cfg;
}

/* Build CFG edges */
static void cfgBuildEdges(CFG* cfg) {
    /* Map labels to blocks */
    /* For each block, determine successors */
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        if (bb->count == 0) continue;

        Inst* last = bb->insts[bb->count - 1];

        if (last->kind == I_GOTO) {
            BasicBlock* target = cfgFindLabel(cfg, last->label);
            if (target) {
                bbAddSucc(bb, target);
                bbAddPred(target, bb);
            }
        } else if (last->kind == I_IF) {
            /* IF has two successors: next block and the target label */
            BasicBlock* target = cfgFindLabel(cfg, last->label);
            if (target) {
                bbAddSucc(bb, target);
                bbAddPred(target, bb);
            }
            /* Fall-through to next block */
            if (i + 1 < cfg->count) {
                bbAddSucc(bb, cfg->blocks[i + 1]);
                bbAddPred(cfg->blocks[i + 1], bb);
            }
        } else if (last->kind == I_RETURN) {
            /* No successors */
        } else {
            /* Fall-through to next block */
            if (i + 1 < cfg->count) {
                bbAddSucc(bb, cfg->blocks[i + 1]);
                bbAddPred(cfg->blocks[i + 1], bb);
            }
        }
    }
}

/* Mark blocks that are in loops (back edges in DFS) */
static void cfgMarkLoops(CFG* cfg) {
    /* Simple: mark blocks that have a successor with lower/equal ID (back edge) */
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        for (int j = 0; j < bb->succCount; j++) {
            if (bb->succs[j]->id <= bb->id) {
                /* Found a back edge - mark all blocks between succ and this as in loop */
                for (int k = bb->succs[j]->id; k <= bb->id && k < cfg->count; k++) {
                    cfg->blocks[k]->inLoop = 1;
                }
            }
        }
    }
}

/* ================================================================
 * Local Optimization within a basic block (DAG-style)
 * ================================================================ */

typedef struct {
    char* key;       /* expression key like "v1 + v2" or "v1" */
    char* resultVar; /* variable that holds this value */
    int isConst;
    int constVal;
} ValueEntry;

typedef struct {
    ValueEntry* entries;
    int count;
    int cap;
} ValueTable;

static ValueTable* vtNew(void) {
    ValueTable* vt = calloc(1, sizeof(ValueTable));
    vt->cap = 32;
    vt->entries = malloc(sizeof(ValueEntry) * vt->cap);
    return vt;
}

static void vtFree(ValueTable* vt) {
    if (!vt) return;
    for (int i = 0; i < vt->count; i++) free(vt->entries[i].key);
    free(vt->entries);
    free(vt);
}

/* Look up an expression key; return result variable name or NULL */
static const char* vtLookup(ValueTable* vt, const char* key) {
    for (int i = 0; i < vt->count; i++)
        if (!strcmp(vt->entries[i].key, key))
            return vt->entries[i].resultVar;
    return NULL;
}

/* Look up constant value for a variable */
static int vtLookupConst(ValueTable* vt, const char* key, int* val) {
    for (int i = 0; i < vt->count; i++)
        if (!strcmp(vt->entries[i].key, key) && vt->entries[i].isConst) {
            *val = vt->entries[i].constVal;
            return 1;
        }
    return 0;
}

/* Add or update an entry */
static void vtSet(ValueTable* vt, const char* key, const char* resultVar) {
    /* First check if key already exists */
    for (int i = 0; i < vt->count; i++) {
        if (!strcmp(vt->entries[i].key, key)) {
            vt->entries[i].resultVar = my_strdup(resultVar);
            vt->entries[i].isConst = 0;
            return;
        }
    }
    /* Add new entry */
    if (vt->count >= vt->cap) {
        vt->cap *= 2;
        vt->entries = realloc(vt->entries, sizeof(ValueEntry) * vt->cap);
    }
    vt->entries[vt->count].key = my_strdup(key);
    vt->entries[vt->count].resultVar = my_strdup(resultVar);
    vt->entries[vt->count].isConst = 0;
    vt->entries[vt->count].constVal = 0;
    vt->count++;
}

static void vtSetConst(ValueTable* vt, const char* key, int val) {
    for (int i = 0; i < vt->count; i++) {
        if (!strcmp(vt->entries[i].key, key)) {
            vt->entries[i].isConst = 1;
            vt->entries[i].constVal = val;
            return;
        }
    }
    if (vt->count >= vt->cap) {
        vt->cap *= 2;
        vt->entries = realloc(vt->entries, sizeof(ValueEntry) * vt->cap);
    }
    vt->entries[vt->count].key = my_strdup(key);
    vt->entries[vt->count].resultVar = my_strdup(key);
    vt->entries[vt->count].isConst = 1;
    vt->entries[vt->count].constVal = val;
    vt->count++;
}

/* Invalidate entries containing a variable */
static void vtKill(ValueTable* vt, const char* var) {
    if (!var || !*var) return;
    for (int i = 0; i < vt->count; i++) {
        /* Check if key contains the variable (as an operand) */
        /* Simple check: key starts with var or contains " var" */
        char* k = vt->entries[i].key;
        if (strcmp(k, var) == 0) {
            /* This variable is being redefined - mark it */
            /* We keep it but update it */
            continue;
        }
        /* If the key is an expression involving var, invalidate it */
        size_t len = strlen(var);
        char* pos = k;
        while ((pos = strstr(pos, var)) != NULL) {
            /* Check word boundaries */
            int beforeOk = (pos == k || isspace((unsigned char)*(pos-1)) ||
                           *(pos-1) == '+' || *(pos-1) == '-' || *(pos-1) == '*' ||
                           *(pos-1) == '/' || *(pos-1) == '(' || *(pos-1) == '&');
            int afterOk = (pos[len] == '\0' || isspace((unsigned char)pos[len]) ||
                          pos[len] == '+' || pos[len] == '-' || pos[len] == '*' ||
                          pos[len] == '/' || pos[len] == ')' || pos[len] == ' ');
            if (beforeOk && afterOk) {
                /* Remove this entry */
                free(vt->entries[i].key);
                vt->entries[i].key = my_strdup("");
                break;
            }
            pos++;
        }
    }
}

/* Build a canonical expression key for the value table.
 * For commutative operations (+, *), we canonicalize operands:
 *   - constants always go second
 *   - for two variables, sort alphabetically (lower name first)
 * This ensures e.g. v1+v2 and v2+v1 map to the same key. */
static void makeExprKey(char* buf, size_t sz, Inst* inst) {
    if (inst->kind == I_ASSIGN) {
        int isCommutative = (!strcmp(inst->op, "+") || !strcmp(inst->op, "*"));
        if (isCommutative) {
            /* Canonicalize: constants go second; for two variables, sort */
            if (isConst(inst->src1) && !isConst(inst->src2)) {
                /* #N op var → var op #N */
                snprintf(buf, sz, "%s %s %s", inst->src2, inst->op, inst->src1);
            } else if (!isConst(inst->src1) && isConst(inst->src2)) {
                /* var op #N → keep as-is */
                snprintf(buf, sz, "%s %s %s", inst->src1, inst->op, inst->src2);
            } else if (!isConst(inst->src1) && !isConst(inst->src2)) {
                /* var op var → sort alphabetically */
                if (strcmp(inst->src1, inst->src2) < 0) {
                    snprintf(buf, sz, "%s %s %s", inst->src1, inst->op, inst->src2);
                } else {
                    snprintf(buf, sz, "%s %s %s", inst->src2, inst->op, inst->src1);
                }
            } else {
                /* Both constants - this shouldn't normally reach CSE */
                snprintf(buf, sz, "%s %s %s", inst->src1, inst->op, inst->src2);
            }
        } else {
            /* Non-commutative: keep original order */
            snprintf(buf, sz, "%s %s %s", inst->src1, inst->op, inst->src2);
        }
    } else if (inst->kind == I_COPY) {
        snprintf(buf, sz, "%s", inst->src1);
    }
}

/* Replace variables in an instruction with their known constant values */
static void replaceKnownConsts(Inst* inst, ValueTable* vt) {
    int val;
    if (isVar(inst->src1) && vtLookupConst(vt, inst->src1, &val)) {
        char newVal[32];
        snprintf(newVal, sizeof(newVal), "#%d", val);
        snprintf(inst->src1, sizeof(inst->src1), "%s", newVal);

        /* Re-evaluate constant folding */
        if (inst->kind == I_ASSIGN && isConst(inst->src1) && isConst(inst->src2)) {
            inst->hasConstVal = 1;
            int v1 = constVal(inst->src1), v2 = constVal(inst->src2);
            if (!strcmp(inst->op, "+")) inst->constVal = v1 + v2;
            else if (!strcmp(inst->op, "-")) inst->constVal = v1 - v2;
            else if (!strcmp(inst->op, "*")) inst->constVal = v1 * v2;
            else if (!strcmp(inst->op, "/")) inst->constVal = (v2 != 0) ? v1 / v2 : 0;
        }
        if (inst->kind == I_COPY) {
            inst->hasConstVal = 1;
            inst->constVal = val;
        }
    }
    if (isVar(inst->src2) && vtLookupConst(vt, inst->src2, &val)) {
        char newVal[32];
        snprintf(newVal, sizeof(newVal), "#%d", val);
        snprintf(inst->src2, sizeof(inst->src2), "%s", newVal);

        if (inst->kind == I_ASSIGN && isConst(inst->src1) && isConst(inst->src2)) {
            inst->hasConstVal = 1;
            int v1 = constVal(inst->src1), v2 = constVal(inst->src2);
            if (!strcmp(inst->op, "+")) inst->constVal = v1 + v2;
            else if (!strcmp(inst->op, "-")) inst->constVal = v1 - v2;
            else if (!strcmp(inst->op, "*")) inst->constVal = v1 * v2;
            else if (!strcmp(inst->op, "/")) inst->constVal = (v2 != 0) ? v1 / v2 : 0;
        }
    }

    /* For I_IF: update hasConstVal if both operands are now constants */
    if (inst->kind == I_IF && isConst(inst->src1) && isConst(inst->src2)) {
        inst->hasConstVal = 1;
        int v1 = constVal(inst->src1), v2 = constVal(inst->src2);
        if (!strcmp(inst->op, "==")) inst->constVal = (v1 == v2);
        else if (!strcmp(inst->op, "!=")) inst->constVal = (v1 != v2);
        else if (!strcmp(inst->op, ">")) inst->constVal = (v1 > v2);
        else if (!strcmp(inst->op, "<")) inst->constVal = (v1 < v2);
        else if (!strcmp(inst->op, ">=")) inst->constVal = (v1 >= v2);
        else if (!strcmp(inst->op, "<=")) inst->constVal = (v1 <= v2);
    }
}

/* Check if an instruction is dead (its dest is never used later in the block) */
static int isInstDeadInBlock(BasicBlock* bb, int idx) {
    Inst* inst = bb->insts[idx];
    const char* dest = inst->dest;
    if (!dest || !*dest) return 0;

    /* Don't kill stores, calls, etc. */
    if (inst->kind == I_STORE || inst->kind == I_CALL ||
        inst->kind == I_READ || inst->kind == I_WRITE ||
        inst->kind == I_RETURN || inst->kind == I_ARG ||
        inst->kind == I_DEC || inst->kind == I_PARAM ||
        inst->kind == I_FUNCTION || inst->kind == I_LABEL ||
        inst->kind == I_GOTO || inst->kind == I_IF)
        return 0;

    /* Check if dest is used in subsequent instructions in this block */
    for (int j = idx + 1; j < bb->count; j++) {
        Inst* later = bb->insts[j];
        /* If dest is redefined before use, it's dead.
         * NOTE: STORE (*x := y) does NOT redefine x; x is the address being
         * written to and its value (the pointer) is unchanged. */
        if (later->kind != I_STORE && later->dest[0] && !strcmp(later->dest, dest)) return 1;
        /* If dest is used, it's live */
        if (later->src1[0] && !strcmp(later->src1, dest)) return 0;
        if (later->src2[0] && !strcmp(later->src2, dest)) return 0;
        /* STORE's dest (the address) is also a use */
        if (later->kind == I_STORE && later->dest[0] && !strcmp(later->dest, dest)) return 0;
    }
    /* Not used after this block - check if it escapes.
       Conservative: assume it's live if it's a named variable (starts with v) */
    if (dest[0] == 'v') return 0;
    /* Temporaries (t*) not used are dead */
    if (dest[0] == 't') return 1;
    return 0;
}

/* ---- Local Constant Folding & Copy Propagation on a basic block ---- */
static void localOptimize(BasicBlock* bb) {
    ValueTable* vt = vtNew();

    for (int i = 0; i < bb->count; i++) {
        Inst* inst = bb->insts[i];

        /* Replace variables with known constant values */
        if (inst->kind == I_ASSIGN || inst->kind == I_COPY || inst->kind == I_IF) {
            replaceKnownConsts(inst, vt);
        }

        if (inst->kind == I_ASSIGN) {
            char key[128];
            makeExprKey(key, sizeof(key), inst);

            /* Check if constant folding applies */
            if (inst->hasConstVal) {
                /* Replace with x := #const */
                inst->kind = I_COPY;
                snprintf(inst->src1, sizeof(inst->src1), "#%d", inst->constVal);
                inst->isCopy = 1;
                snprintf(inst->copySrc, sizeof(inst->copySrc), "#%d", inst->constVal);
                vtSetConst(vt, inst->dest, inst->constVal);
                vtKill(vt, inst->dest);
                vtSet(vt, inst->dest, inst->dest);
                vtSetConst(vt, inst->dest, inst->constVal);
                continue;
            }

            /* Check for common subexpression */
            const char* existing = vtLookup(vt, key);
            if (existing && strcmp(existing, inst->dest) != 0) {
                /* Replace with copy from the variable that already holds this value */
                inst->kind = I_COPY;
                snprintf(inst->src1, sizeof(inst->src1), "%s", existing);
                inst->isCopy = 1;
                snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", existing);
                inst->hasConstVal = 0;

                /* Kill old dest entries and register dest as a copy source */
                vtKill(vt, inst->dest);
                vtSet(vt, inst->dest, inst->dest);
                /* Do NOT update the expression key - keep it pointing to the
                 * earliest computation of this value for cleaner output. */
                continue;
            }

            /* No CSE match: register this expression and the dest variable */
            vtKill(vt, inst->dest);
            vtSet(vt, inst->dest, inst->dest);
            vtSet(vt, key, inst->dest);
        } else if (inst->kind == I_COPY) {
            /* If copying from a constant, mark dest as constant */
            if (isConst(inst->src1)) {
                vtKill(vt, inst->dest);
                vtSet(vt, inst->dest, inst->dest);
                vtSetConst(vt, inst->dest, constVal(inst->src1));
            } else {
                /* Check if src1 holds a constant */
                int cval;
                if (vtLookupConst(vt, inst->src1, &cval)) {
                    snprintf(inst->src1, sizeof(inst->src1), "#%d", cval);
                    inst->hasConstVal = 1;
                    inst->constVal = cval;
                }
                vtKill(vt, inst->dest);
                vtSet(vt, inst->dest, inst->dest);
                /* Propagate constant if known */
                if (isConst(inst->src1)) {
                    vtSetConst(vt, inst->dest, constVal(inst->src1));
                }
            }
        } else if (inst->kind == I_IF) {
            /* If both operands are constants, we could fold the branch.
               This is handled elsewhere (dead branch elimination). */
        } else if (inst->kind == I_READ) {
            vtKill(vt, inst->dest);
            vtSet(vt, inst->dest, inst->dest);
        }
    }

    vtFree(vt);

    /* ---- Dead code elimination within block ---- */
    int newCount = 0;
    for (int i = 0; i < bb->count; i++) {
        if (!isInstDeadInBlock(bb, i)) {
            if (newCount != i) bb->insts[newCount] = bb->insts[i];
            newCount++;
        } else {
            instFree(bb->insts[i]);
        }
    }
    bb->count = newCount;
}

/* ================================================================
 * Global Constant Propagation (simple)
 * ================================================================ */

typedef struct {
    char* var;
    int isConst;
    int val;
} GlobalConst;

static GlobalConst* gConsts = NULL;
static int gConstCount = 0;

static int gLookupConst(const char* var, int* val) {
    for (int i = 0; i < gConstCount; i++) {
        if (!strcmp(gConsts[i].var, var) && gConsts[i].isConst) {
            *val = gConsts[i].val;
            return 1;
        }
    }
    return 0;
}

static void gSetConst(const char* var, int val) {
    for (int i = 0; i < gConstCount; i++) {
        if (!strcmp(gConsts[i].var, var)) {
            gConsts[i].isConst = 1;
            gConsts[i].val = val;
            return;
        }
    }
    gConsts = realloc(gConsts, sizeof(GlobalConst) * (gConstCount + 1));
    gConsts[gConstCount].var = my_strdup(var);
    gConsts[gConstCount].isConst = 1;
    gConsts[gConstCount].val = val;
    gConstCount++;
}

static void gKillConst(const char* var) {
    for (int i = 0; i < gConstCount; i++) {
        if (!strcmp(gConsts[i].var, var)) {
            gConsts[i].isConst = 0;
            return;
        }
    }
}

static void gClearConsts(void) {
    for (int i = 0; i < gConstCount; i++) free(gConsts[i].var);
    free(gConsts);
    gConsts = NULL;
    gConstCount = 0;
}

/* Apply global constant propagation to a CFG.
 * NOTE: simple forward propagation; may be imprecise at merge points.
 * Disabled in the main pipeline; kept for reference. */
__attribute__((unused))
static void globalConstProp(CFG* cfg) {
    gClearConsts();
    int changed = 1;
    int maxIter = 5;

    while (changed && maxIter-- > 0) {
        changed = 0;
        for (int i = 0; i < cfg->count; i++) {
            BasicBlock* bb = cfg->blocks[i];
            for (int j = 0; j < bb->count; j++) {
                Inst* inst = bb->insts[j];
                int cval;

                /* Replace vars with known constants */
                if (inst->kind == I_ASSIGN || inst->kind == I_COPY || inst->kind == I_IF) {
                    if (isVar(inst->src1) && gLookupConst(inst->src1, &cval)) {
                        char buf[32];
                        snprintf(buf, sizeof(buf), "#%d", cval);
                        snprintf(inst->src1, sizeof(inst->src1), "%s", buf);
                        changed = 1;

                        /* Re-fold */
                        if (inst->kind == I_ASSIGN && isConst(inst->src1) && isConst(inst->src2)) {
                            inst->hasConstVal = 1;
                            int v1 = constVal(inst->src1), v2 = constVal(inst->src2);
                            if (!strcmp(inst->op, "+")) inst->constVal = v1 + v2;
                            else if (!strcmp(inst->op, "-")) inst->constVal = v1 - v2;
                            else if (!strcmp(inst->op, "*")) inst->constVal = v1 * v2;
                            else if (!strcmp(inst->op, "/")) inst->constVal = (v2 != 0) ? v1 / v2 : 0;
                        }
                        if (inst->kind == I_COPY && isConst(inst->src1)) {
                            inst->hasConstVal = 1;
                            inst->constVal = cval;
                        }
                    }
                    if (inst->kind == I_ASSIGN && isVar(inst->src2) && gLookupConst(inst->src2, &cval)) {
                        char buf[32];
                        snprintf(buf, sizeof(buf), "#%d", cval);
                        snprintf(inst->src2, sizeof(inst->src2), "%s", buf);
                        changed = 1;

                        if (isConst(inst->src1) && isConst(inst->src2)) {
                            inst->hasConstVal = 1;
                            int v1 = constVal(inst->src1), v2 = constVal(inst->src2);
                            if (!strcmp(inst->op, "+")) inst->constVal = v1 + v2;
                            else if (!strcmp(inst->op, "-")) inst->constVal = v1 - v2;
                            else if (!strcmp(inst->op, "*")) inst->constVal = v1 * v2;
                            else if (!strcmp(inst->op, "/")) inst->constVal = (v2 != 0) ? v1 / v2 : 0;
                        }
                    }
                }

                /* Track constant assignments */
                if (inst->hasConstVal && inst->dest[0]) {
                    gSetConst(inst->dest, inst->constVal);
                } else if (inst->kind == I_COPY && isConst(inst->src1) && inst->dest[0]) {
                    gSetConst(inst->dest, constVal(inst->src1));
                } else if (inst->kind == I_ASSIGN || inst->kind == I_COPY || inst->kind == I_READ) {
                    if (inst->dest[0]) gKillConst(inst->dest);
                }
            }
        }
    }
    gClearConsts();
}

/* ================================================================
 * Global Dead Code Elimination
 * ================================================================ */

/* Mark instructions reachable from CFG entry; remove unreachable blocks */
static void cfgRemoveUnreachable(CFG* cfg) {
    if (cfg->count == 0) return;

    /* Reset visited flags */
    for (int i = 0; i < cfg->count; i++) cfg->blocks[i]->visited = 0;

    /* BFS from every FUNCTION block (each function is a separate entry point) */
    BasicBlock** queue = malloc(sizeof(BasicBlock*) * cfg->count);
    int head = 0, tail = 0;

    for (int i = 0; i < cfg->count; i++) {
        if (cfg->blocks[i]->count > 0 &&
            cfg->blocks[i]->insts[0]->kind == I_FUNCTION) {
            queue[tail++] = cfg->blocks[i];
            cfg->blocks[i]->visited = 1;
        }
    }
    /* If no FUNCTION blocks found, start from block 0 */
    if (tail == 0) {
        queue[tail++] = cfg->blocks[0];
        cfg->blocks[0]->visited = 1;
    }

    while (head < tail) {
        BasicBlock* bb = queue[head++];
        for (int i = 0; i < bb->succCount; i++) {
            if (!bb->succs[i]->visited) {
                bb->succs[i]->visited = 1;
                queue[tail++] = bb->succs[i];
            }
        }
    }
    free(queue);

    /* Remove unreachable blocks */
    int newCount = 0;
    for (int i = 0; i < cfg->count; i++) {
        if (cfg->blocks[i]->visited) {
            if (newCount != i) cfg->blocks[newCount] = cfg->blocks[i];
            newCount++;
        } else {
            bbFree(cfg->blocks[i]);
        }
    }
    cfg->count = newCount;
}

/* Check if a variable is used in any block reachable from the given set of
 * start blocks, considering only uses BEFORE any redefinition. */
static int isVarUsedInReachable(BasicBlock** starts, int startCount,
                                 BasicBlock* currentBB, int afterIdx,
                                 const char* var) {
    if (!var || !*var) return 0;

    /* BFS through reachable blocks */
    int qcap = 128;
    BasicBlock** queue = malloc(sizeof(BasicBlock*) * qcap);
    int qhead = 0, qtail = 0;
    int* visited = calloc(qcap, sizeof(int));

    /* Process each start block: check for uses/redefinitions */
    for (int s = 0; s < startCount; s++) {
        BasicBlock* sb = starts[s];
        int isCur = (sb == currentBB);
        int startFrom = isCur ? afterIdx + 1 : 0;
        int killed = 0;

        for (int k = startFrom; k < sb->count; k++) {
            Inst* inst = sb->insts[k];
            /* If redefined before use, stop this path.
             * STORE (*x := y) does NOT redefine x; x is the address. */
            if (inst->kind != I_STORE && inst->dest[0] && !strcmp(inst->dest, var)) {
                killed = 1;
                break;
            }
            /* If used as source operand, it's live */
            if (inst->src1[0] && !strcmp(inst->src1, var))
                { free(queue); free(visited); return 1; }
            if (inst->src2[0] && !strcmp(inst->src2, var))
                { free(queue); free(visited); return 1; }
            /* Used in WRITE, RETURN, ARG */
            if ((inst->kind == I_WRITE || inst->kind == I_RETURN || inst->kind == I_ARG) &&
                inst->src1[0] && !strcmp(inst->src1, var))
                { free(queue); free(visited); return 1; }
            /* Used as STORE target (address) */
            if (inst->kind == I_STORE && inst->dest[0] && !strcmp(inst->dest, var))
                { free(queue); free(visited); return 1; }
        }

        /* If not killed in this start block, add its successors to the queue */
        if (!killed) {
            for (int t = 0; t < sb->succCount; t++) {
                BasicBlock* succ = sb->succs[t];
                int sid = succ->id;
                if (sid >= qcap) {
                    int ncap = sid + 64;
                    visited = realloc(visited, sizeof(int) * ncap);
                    for (int x = qcap; x < ncap; x++) visited[x] = 0;
                    qcap = ncap;
                }
                if (!visited[sid]) {
                    visited[sid] = 1;
                    if (qtail >= qcap) {
                        qcap *= 2;
                        queue = realloc(queue, sizeof(BasicBlock*) * qcap);
                        visited = realloc(visited, sizeof(int) * qcap);
                    }
                    queue[qtail++] = succ;
                }
            }
        }
    }

    /* BFS through successor blocks */
    while (qhead < qtail) {
        BasicBlock* bb = queue[qhead++];
        int killed = 0;
        for (int k = 0; k < bb->count; k++) {
            Inst* inst = bb->insts[k];
            if (inst->kind != I_STORE && inst->dest[0] && !strcmp(inst->dest, var)) {
                killed = 1;
                break;
            }
            if (inst->src1[0] && !strcmp(inst->src1, var))
                { free(queue); free(visited); return 1; }
            if (inst->src2[0] && !strcmp(inst->src2, var))
                { free(queue); free(visited); return 1; }
            if ((inst->kind == I_WRITE || inst->kind == I_RETURN || inst->kind == I_ARG) &&
                inst->src1[0] && !strcmp(inst->src1, var))
                { free(queue); free(visited); return 1; }
            if (inst->kind == I_STORE && inst->dest[0] && !strcmp(inst->dest, var))
                { free(queue); free(visited); return 1; }
        }
        if (!killed) {
            for (int t = 0; t < bb->succCount; t++) {
                BasicBlock* succ = bb->succs[t];
                int sid = succ->id;
                if (sid >= qcap) {
                    int ncap = sid + 64;
                    visited = realloc(visited, sizeof(int) * ncap);
                    for (int x = qcap; x < ncap; x++) visited[x] = 0;
                    qcap = ncap;
                }
                if (!visited[sid]) {
                    visited[sid] = 1;
                    if (qtail >= qcap) {
                        qcap *= 2;
                        queue = realloc(queue, sizeof(BasicBlock*) * qcap);
                        visited = realloc(visited, sizeof(int) * qcap);
                    }
                    queue[qtail++] = succ;
                }
            }
        }
    }

    free(queue);
    free(visited);
    return 0;
}

/* Eliminate dead assignments using liveness analysis across blocks. */
static void globalDeadCodeElim(CFG* cfg) {
    if (cfg->count == 0) return;

    for (int iter = 0; iter < 3; iter++) {
        for (int i = cfg->count - 1; i >= 0; i--) {
            BasicBlock* bb = cfg->blocks[i];
            int newCount = 0;

            for (int j = 0; j < bb->count; j++) {
                Inst* inst = bb->insts[j];

                int isDead = 0;
                if (inst->dest[0] && inst->kind != I_STORE && inst->kind != I_CALL &&
                    inst->kind != I_READ && inst->kind != I_WRITE &&
                    inst->kind != I_RETURN && inst->kind != I_ARG &&
                    inst->kind != I_DEC && inst->kind != I_PARAM &&
                    inst->kind != I_FUNCTION && inst->kind != I_LABEL &&
                    inst->kind != I_GOTO && inst->kind != I_IF) {

                    const char* dest = inst->dest;
                    int redefined = 0, used = 0;

                    /* Check later instructions in THIS block */
                    for (int k = j + 1; k < bb->count; k++) {
                        Inst* later = bb->insts[k];
                        /* STORE (*x := y) does NOT redefine x; x is the address */
                        if (later->kind != I_STORE && later->dest[0] &&
                            !strcmp(later->dest, dest))
                            { redefined = 1; break; }
                        if (later->src1[0] && !strcmp(later->src1, dest))
                            { used = 1; break; }
                        if (later->src2[0] && !strcmp(later->src2, dest))
                            { used = 1; break; }
                        if ((later->kind == I_WRITE || later->kind == I_RETURN ||
                             later->kind == I_ARG) &&
                            later->src1[0] && !strcmp(later->src1, dest))
                            { used = 1; break; }
                        if (later->kind == I_STORE && later->dest[0] &&
                            !strcmp(later->dest, dest))
                            { used = 1; break; }
                    }

                    if (redefined) {
                        isDead = 1;
                    } else if (used) {
                        isDead = 0;
                    } else {
                        /* Not used/redefined in this block - check reachable blocks */
                        int sc = bb->succCount;
                        BasicBlock** starts = NULL;
                        if (sc > 0) {
                            starts = malloc(sizeof(BasicBlock*) * sc);
                            for (int s = 0; s < sc; s++)
                                starts[s] = bb->succs[s];
                        }
                        /* Start from successors; if none, it's dead */
                        isDead = (sc == 0) ? 1 :
                                 !isVarUsedInReachable(starts, sc, bb, j, dest);
                        free(starts);
                    }
                }

                if (!isDead) {
                    if (newCount != j) bb->insts[newCount] = bb->insts[j];
                    newCount++;
                } else {
                    instFree(bb->insts[j]);
                }
            }
            bb->count = newCount;
        }
    }
}

/* ================================================================
 * Global Common Subexpression Elimination
 * ================================================================ */

static void globalCSE(CFG* cfg) {
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        localOptimize(bb); /* Re-run local optimization which includes CSE */
    }
}

/* ================================================================
 * Loop-Invariant Code Motion
 * ================================================================ */

static int isInstLoopInvariant(CFG* cfg, Inst* inst, BasicBlock* bb) {
    if (!inst->dest[0]) return 0;
    if (inst->kind != I_ASSIGN && inst->kind != I_COPY) return 0;

    /* Check that all operands are constants or defined outside the loop */
    if (isVar(inst->src1) && inst->src1[0] != '#') {
        for (int i = 0; i < cfg->count; i++) {
            if (!cfg->blocks[i]->inLoop) continue;
            BasicBlock* loopBB = cfg->blocks[i];
            for (int j = 0; j < loopBB->count; j++) {
                if (loopBB->insts[j]->dest[0] &&
                    !strcmp(loopBB->insts[j]->dest, inst->src1))
                    return 0;
            }
        }
    }
    if (inst->kind == I_ASSIGN && isVar(inst->src2) && inst->src2[0] != '#') {
        for (int i = 0; i < cfg->count; i++) {
            if (!cfg->blocks[i]->inLoop) continue;
            BasicBlock* loopBB = cfg->blocks[i];
            for (int j = 0; j < loopBB->count; j++) {
                if (loopBB->insts[j]->dest[0] &&
                    !strcmp(loopBB->insts[j]->dest, inst->src2))
                    return 0;
            }
        }
    }
    return 1;
}

static void loopInvariantMotion(CFG* cfg) {
    /* Find loop pre-headers (blocks just before loop headers) */
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        if (!bb->inLoop) continue;

        /* Check if this is a loop header (has a back-edge predecessor) */
        int isHeader = 0;
        for (int j = 0; j < bb->predCount; j++) {
            if (bb->preds[j]->id >= bb->id) { isHeader = 1; break; }
        }
        if (!isHeader) continue;

        /* Find pre-header (predecessor outside loop) */
        BasicBlock* preheader = NULL;
        for (int j = 0; j < bb->predCount; j++) {
            if (!bb->preds[j]->inLoop) { preheader = bb->preds[j]; break; }
        }
        if (!preheader) continue;

        /* Move loop-invariant instructions from this block to pre-header */
        int newCount = 0;
        for (int j = 0; j < bb->count; j++) {
            Inst* inst = bb->insts[j];
            if (inst->kind == I_LABEL) {
                if (newCount != j) bb->insts[newCount] = bb->insts[j];
                newCount++;
                continue;
            }
            if (isInstLoopInvariant(cfg, inst, bb)) {
                /* Move to pre-header (insert before terminator) */
                int insertPos = preheader->count;
                Inst* last = preheader->insts[preheader->count - 1];
                if (last && (last->kind == I_GOTO || last->kind == I_IF || last->kind == I_RETURN))
                    insertPos = preheader->count - 1;

                /* Shift pre-header instructions */
                if (preheader->count >= preheader->cap) {
                    preheader->cap *= 2;
                    preheader->insts = realloc(preheader->insts, sizeof(Inst*) * preheader->cap);
                }
                for (int k = preheader->count; k > insertPos; k--)
                    preheader->insts[k] = preheader->insts[k - 1];
                preheader->insts[insertPos] = inst;
                preheader->count++;
                /* Don't add to current block's new list */
            } else {
                if (newCount != j) bb->insts[newCount] = bb->insts[j];
                newCount++;
            }
        }
        bb->count = newCount;
    }
}

/* ================================================================
 * Strength Reduction
 * ================================================================ */

static void strengthReduction(CFG* cfg) {
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        for (int j = 0; j < bb->count; j++) {
            Inst* inst = bb->insts[j];
            if (inst->kind != I_ASSIGN) continue;

            /* Replace multiplication by power of 2 with shift (not supported in our
               target IR, but we can replace *2 with +, *4 with << etc.) */
            /* Actually, our MIPS backend doesn't support shifts explicitly in IR,
               but we can do simple strength reductions:
               x := y * #1  ==>  x := y
               x := y * #0  ==>  x := #0
               x := y + #0  ==>  x := y
               x := y - #0  ==>  x := y
               x := #0 - y  ==>  x := #-1 * y (keep as is)
            */
            if (!strcmp(inst->op, "*") && isConst(inst->src2)) {
                int v = constVal(inst->src2);
                if (v == 0) {
                    inst->kind = I_COPY;
                    snprintf(inst->src1, sizeof(inst->src1), "#0");
                    inst->hasConstVal = 1;
                    inst->constVal = 0;
                    inst->isCopy = 1;
                    snprintf(inst->copySrc, sizeof(inst->copySrc), "#0");
                } else if (v == 1 && isVar(inst->src1)) {
                    inst->kind = I_COPY;
                    inst->hasConstVal = 0;
                    inst->isCopy = 1;
                    snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", inst->src1);
                } else if (v == 2) {
                    /* Keep *2 but could be replaced with + */
                    /* For simplicity, replace y * 2 with y + y */
                    /* Actually, don't do this as it can increase register pressure */
                }
            }
            if (!strcmp(inst->op, "+") && isConst(inst->src2) && constVal(inst->src2) == 0 && isVar(inst->src1)) {
                inst->kind = I_COPY;
                inst->hasConstVal = 0;
                inst->isCopy = 1;
                snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", inst->src1);
            }
            if (!strcmp(inst->op, "-") && isConst(inst->src2) && constVal(inst->src2) == 0 && isVar(inst->src1)) {
                inst->kind = I_COPY;
                inst->hasConstVal = 0;
                inst->isCopy = 1;
                snprintf(inst->copySrc, sizeof(inst->copySrc), "%s", inst->src1);
            }
        }
    }
}

/* ================================================================
 * Dead Branch Elimination
 * ================================================================ */

static void deadBranchElim(CFG* cfg) {
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        if (bb->count == 0) continue;

        Inst* last = bb->insts[bb->count - 1];
        if (last->kind == I_IF && isConst(last->src1) && isConst(last->src2)) {
            int v1 = constVal(last->src1), v2 = constVal(last->src2);
            int result = 0;
            if (!strcmp(last->op, "==")) result = (v1 == v2);
            else if (!strcmp(last->op, "!=")) result = (v1 != v2);
            else if (!strcmp(last->op, ">")) result = (v1 > v2);
            else if (!strcmp(last->op, "<")) result = (v1 < v2);
            else if (!strcmp(last->op, ">=")) result = (v1 >= v2);
            else if (!strcmp(last->op, "<=")) result = (v1 <= v2);

            /* Replace IF + GOTO with direct GOTO or remove */
            if (result) {
                /* Branch always taken: replace IF with GOTO to target */
                last->kind = I_GOTO;
            } else {
                /* Branch never taken: remove the IF instruction */
                instFree(bb->insts[bb->count - 1]);
                bb->count--;
            }
        }
    }

    /* Rebuild edges after potential changes */
    cfgRemoveUnreachable(cfg);
}

/* ================================================================
 * Peephole / local cleanup
 * ================================================================ */

static void peepholeCleanup(CFG* cfg) {
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        ValueTable* vt = vtNew();

        for (int j = 0; j < bb->count; j++) {
            Inst* inst = bb->insts[j];

            /* Eliminate self-copies: x := x */
            if (inst->kind == I_COPY && isVar(inst->src1) &&
                isVar(inst->dest) && !strcmp(inst->dest, inst->src1)) {
                /* Mark for deletion by zeroing out */
                inst->kind = I_NONE;
                continue;
            }

            /* Replace variable references with known constants */
            if (inst->kind == I_ASSIGN || inst->kind == I_COPY || inst->kind == I_IF) {
                replaceKnownConsts(inst, vt);
            }

            /* Track variable values */
            if (inst->kind == I_COPY && inst->dest[0]) {
                vtKill(vt, inst->dest);
                vtSet(vt, inst->dest, inst->dest);
                if (isConst(inst->src1)) {
                    vtSetConst(vt, inst->dest, constVal(inst->src1));
                }
            } else if (inst->kind == I_ASSIGN && inst->dest[0]) {
                vtKill(vt, inst->dest);
                vtSet(vt, inst->dest, inst->dest);
            } else if (inst->kind == I_READ && inst->dest[0]) {
                vtKill(vt, inst->dest);
                vtSet(vt, inst->dest, inst->dest);
            }
        }
        vtFree(vt);

        /* Remove I_NONE instructions */
        int newCount = 0;
        for (int j = 0; j < bb->count; j++) {
            if (bb->insts[j]->kind != I_NONE) {
                if (newCount != j) bb->insts[newCount] = bb->insts[j];
                newCount++;
            } else {
                instFree(bb->insts[j]);
            }
        }
        bb->count = newCount;
    }

    /* Remove empty blocks (except FUNCTION/LABEL-only blocks) and merge */
    /* (simplified: just remove empty blocks with no label) */
    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        if (bb->count == 0 && bb->label[0] == '\0' && bb->predCount == 0 && bb->succCount == 0) {
            /* Just mark it; will be skipped during IR reconstruction */
            bb->count = -1;
        }
    }
}

/* ================================================================
 * Reconstruct IRCode linked list from CFG
 * ================================================================ */

static IRCode* cfgToIR(CFG* cfg) {
    IRCode* head = NULL;
    IRCode** tail = &head;

    for (int i = 0; i < cfg->count; i++) {
        BasicBlock* bb = cfg->blocks[i];
        if (bb->count <= 0) continue;

        for (int j = 0; j < bb->count; j++) {
            Inst* inst = bb->insts[j];
            char buf[256];

            switch (inst->kind) {
                case I_FUNCTION:
                    snprintf(buf, sizeof(buf), "FUNCTION %s :", inst->dest);
                    break;
                case I_PARAM:
                    snprintf(buf, sizeof(buf), "PARAM %s", inst->dest);
                    break;
                case I_DEC:
                    snprintf(buf, sizeof(buf), "DEC %s %d", inst->dest, inst->constVal);
                    break;
                case I_LABEL:
                    snprintf(buf, sizeof(buf), "LABEL %s :", inst->label);
                    break;
                case I_GOTO:
                    snprintf(buf, sizeof(buf), "GOTO %s", inst->label);
                    break;
                case I_IF:
                    snprintf(buf, sizeof(buf), "IF %s %s %s GOTO %s",
                             inst->src1, inst->op, inst->src2, inst->label);
                    break;
                case I_READ:
                    snprintf(buf, sizeof(buf), "READ %s", inst->dest);
                    break;
                case I_WRITE:
                    snprintf(buf, sizeof(buf), "WRITE %s", inst->src1);
                    break;
                case I_RETURN:
                    snprintf(buf, sizeof(buf), "RETURN %s", inst->src1);
                    break;
                case I_ARG:
                    snprintf(buf, sizeof(buf), "ARG %s", inst->src1);
                    break;
                case I_ASSIGN:
                    snprintf(buf, sizeof(buf), "%s := %s %s %s",
                             inst->dest, inst->src1, inst->op, inst->src2);
                    break;
                case I_COPY:
                    snprintf(buf, sizeof(buf), "%s := %s", inst->dest, inst->src1);
                    break;
                case I_STORE:
                    snprintf(buf, sizeof(buf), "*%s := %s", inst->dest, inst->src1);
                    break;
                case I_LOAD:
                    snprintf(buf, sizeof(buf), "%s := *%s", inst->dest, inst->src1);
                    break;
                case I_ADDR:
                    snprintf(buf, sizeof(buf), "%s := &%s", inst->dest, inst->src1);
                    break;
                case I_CALL:
                    if (inst->dest[0])
                        snprintf(buf, sizeof(buf), "%s := CALL %s", inst->dest, inst->src1);
                    else
                        snprintf(buf, sizeof(buf), "CALL %s", inst->src1);
                    break;
                default:
                    if (inst->line && inst->line[0])
                        snprintf(buf, sizeof(buf), "%s", inst->line);
                    else
                        buf[0] = '\0';
                    break;
            }

            if (buf[0]) {
                IRCode* c = malloc(sizeof(IRCode));
                c->code = my_strdup(buf);
                c->next = NULL;
                *tail = c;
                tail = &c->next;
            }
        }
    }
    return head;
}

/* ================================================================
 * Copy propagation: within each basic block, replace uses of a variable
 * with its copy source when the variable is a simple copy and is not
 * redefined between the copy and the use.
 * ================================================================ */

static void copyPropagation(CFG* cfg) {
    for (int b = 0; b < cfg->count; b++) {
        BasicBlock* bb = cfg->blocks[b];
        /* Build map: var -> what it's a copy of, valid only within this block */
        typedef struct { char* dest; char* src; } CopyEntry;
        CopyEntry* copies = NULL;
        int copyCount = 0;

        for (int i = 0; i < bb->count; i++) {
            Inst* inst = bb->insts[i];

            /* Try to replace src1/src2 with their copy source (local only) */
            if (inst->kind == I_ASSIGN || inst->kind == I_COPY || inst->kind == I_IF ||
                inst->kind == I_WRITE || inst->kind == I_RETURN || inst->kind == I_ARG ||
                inst->kind == I_STORE) {

                if (isVar(inst->src1)) {
                    for (int k = 0; k < copyCount; k++) {
                        if (!strcmp(inst->src1, copies[k].dest)) {
                            snprintf(inst->src1, sizeof(inst->src1), "%s", copies[k].src);
                            break;
                        }
                    }
                }
                if (isVar(inst->src2)) {
                    for (int k = 0; k < copyCount; k++) {
                        if (!strcmp(inst->src2, copies[k].dest)) {
                            snprintf(inst->src2, sizeof(inst->src2), "%s", copies[k].src);
                            break;
                        }
                    }
                }
            }

            /* Kill copies where dest or src is redefined */
            if (inst->dest[0]) {
                int newCount = 0;
                for (int k = 0; k < copyCount; k++) {
                    if (strcmp(copies[k].dest, inst->dest) &&
                        strcmp(copies[k].src, inst->dest)) {
                        copies[newCount] = copies[k];
                        newCount++;
                    } else {
                        free(copies[k].dest);
                        free(copies[k].src);
                    }
                }
                copyCount = newCount;
            }

            /* If this is a copy of a non-address variable, record it */
            if (inst->isCopy && isVar(inst->src1) && isVar(inst->dest) &&
                inst->src1[0] != '&') {
                copies = realloc(copies, sizeof(CopyEntry) * (copyCount + 1));
                copies[copyCount].dest = my_strdup(inst->dest);
                copies[copyCount].src = my_strdup(inst->src1);
                copyCount++;
            }
        }
        for (int k = 0; k < copyCount; k++) {
            free(copies[k].dest);
            free(copies[k].src);
        }
        free(copies);
    }
}

/* ================================================================
 * Main optimization pipeline
 * ================================================================ */

IRCode* optimizeIR(IRCode* ir) {
    if (!ir) return NULL;

    /* Build CFG */
    CFG* cfg = buildCFG(ir);
    cfgBuildEdges(cfg);
    cfgMarkLoops(cfg);

    /* ---- Optimization Pipeline ---- */

    /* Pass 1: Local optimization (CSE, constant folding, copy prop, dead code)
     *         on each basic block */
    for (int i = 0; i < cfg->count; i++) {
        localOptimize(cfg->blocks[i]);
    }

    /* Pass 2: Copy propagation (local to each basic block) */
    copyPropagation(cfg);

    /* Pass 3: Global constant propagation with iterative analysis
     *         (safe: only propagates when all reaching defs agree) */
    /* Skip aggressive global const prop; local const folding suffices */

    /* Pass 4: Global CSE (re-runs local optimization) */
    globalCSE(cfg);

    /* Pass 5: Dead branch elimination */
    deadBranchElim(cfg);

    /* Pass 6: Dead code elimination */
    globalDeadCodeElim(cfg);

    /* Pass 7: Loop-invariant code motion */
    loopInvariantMotion(cfg);

    /* Pass 8: Strength reduction */
    strengthReduction(cfg);

    /* Pass 9: Peephole cleanup */
    peepholeCleanup(cfg);

    /* Final cleanup passes */
    copyPropagation(cfg);
    for (int i = 0; i < cfg->count; i++) {
        localOptimize(cfg->blocks[i]);
    }
    peepholeCleanup(cfg);

    /* Reconstruct IR */
    IRCode* optimized = cfgToIR(cfg);

    cfgFree(cfg);
    return optimized;
}
