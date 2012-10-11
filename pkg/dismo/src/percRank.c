/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "Rmath.h"



SEXP percRank(SEXP x, SEXP y, SEXP tail) {
					
	R_len_t i, j;
	SEXP val;
	double *px, *py, *pval;
	int tailopt, b, t;
	
	PROTECT(x = coerceVector(x, REALSXP));
	PROTECT(y = coerceVector(y, REALSXP));
	tailopt = INTEGER(tail)[0];

	px = REAL(x);
	py = REAL(y);

	int n = length(y);
	int m = length(x);
	
	PROTECT( val = allocVector(REALSXP, n) );
	pval = REAL(val);
	
	for (i=0; i<n; i++) {
		b = 0;
		t = 0;
		for (j=0; j<m; j++) {
			if (py[i] < px[j]) {
				b++;
			} else if (py[i] == px[j]) {
				t++;
			}
		}
		pval[i] = (b + 0.5 * t) / m;
		if (tailopt == 1) { // both
			if (pval[i] > 0.5) {
				pval[i] = 2 * (1 - pval[i]); 
			} else {
				pval[i] = 2 * pval[i];
			}
		
		} else if (tailopt == 2) { // high
			if (pval[i] < 0.5) {
				pval[i] = 1;
			} else {
				pval[i] = 2 * (1 - pval[i]);
			}
		} else if (tailopt == 3) { // low
			if (pval[i] > 0.5) {
				pval[i] = 1;
			} else {
				pval[i] = 2 * pval[i];
			}
		}
	} 
	
	UNPROTECT(3);
	return(val);
}
		