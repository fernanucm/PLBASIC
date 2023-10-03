/***********************************************************************
 * Computation of the closure of a fuzzy relation
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <conio.h>
#include <stdio.h>

/***********************************************************************/



/***********************************************************************
 * Declarations
 ***********************************************************************/

/***********************************************************************
 * Public functions
 ***********************************************************************/

foreign_t pl_pkbhit(term_t tCode) {
  // int nValue;
  intptr_t nCode;
  
  // nValue = _kbhit();
  if ( _kbhit() ) 
  { //nValue = _getch();
    //nCode = nValue;
    nCode = _getch();
    printf("Valor: %d\n", nCode);
  } else {
    nCode = 0;
  };

	if (PL_unify_integer(tCode, nCode)) {
  	PL_succeed;
	} else {
		PL_fail;
  };
}

