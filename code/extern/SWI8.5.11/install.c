/***********************************************************************
 * Foreign library installer
 */

#include <SWI-Prolog.h>

/***********************************************************************/



/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Foreign predicate definitions */
extern foreign_t pl_closure(term_t tInputEquations, term_t tClosure,
                            term_t tTNorm, term_t tRelationName, term_t tLambdaCut,
                            term_t tOutputEquations);

extern foreign_t pl_pkbhit(term_t tCode);





/***********************************************************************
 * Public functions
 ***********************************************************************/


/** install
 *
 *    Registers foreign predicates in SWI-Prolog system. This function
 *    is automatically called when load_foreign_library/1 is used.
 */
install_t install() {
	PL_register_foreign("ext_closure", 6, pl_closure, 0);
	PL_register_foreign("pkbhit", 1, pl_pkbhit, 0);
};


