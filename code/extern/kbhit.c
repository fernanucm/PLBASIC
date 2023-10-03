/***********************************************************************
 * Reading the pressed key code (if any) without waiting
 * Return the key code if a key was pressed or 0 otherwise
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>

/***********************************************************************/


/***********************************************************************
 * Declarations
 ***********************************************************************/

/***********************************************************************
 * Public functions
 ***********************************************************************/

foreign_t pl_kbhit(term_t tCode) {
  intptr_t nCode;
  
  nCode = 0;
  
  while( kbhit() )
     nCode = getch();
     
	if (PL_unify_integer(tCode, nCode)) {
  	PL_succeed;
	} else {
		PL_fail;
  };
}

