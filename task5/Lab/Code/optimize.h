#ifndef OPTIMIZE_H
#define OPTIMIZE_H

#include "ir.h"

/* Run the optimization pipeline on the IR linked list.
 * Returns the optimized IR (may be a new list or the same list modified).
 * The input list is consumed and should not be used after this call.
 */
IRCode* optimizeIR(IRCode* ir);

#endif /* OPTIMIZE_H */
