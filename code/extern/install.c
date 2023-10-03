/***********************************************************************
 * Foreign library installer
 */

#include <SWI-Prolog.h>

/***********************************************************************/


/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Foreign predicate definitions */
extern foreign_t pl_kbhit(term_t tCode);


/***********************************************************************
 * Public functions
 ***********************************************************************/

/** install
 *
 *    Registers foreign predicates in SWI-Prolog system. This function
 *    is automatically called when load_foreign_library/1 is used.
 */
install_t install() {
	PL_register_foreign("kbhit", 1, pl_kbhit, 0);
};


