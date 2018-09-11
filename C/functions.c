#include <R.h>
#include <Rinternals.h>

SEXP vectorAppend(SEXP list, SEXP x) {
  SEXP new; int i;
  int n = length(list);
  PROTECT(new=allocVector(VECSXP, n+1));
  for(i=0;i<n;i++)
    SET_VECTOR_ELT(new, i, Rf_duplicate(VECTOR_ELT(list, i)));
  SET_VECTOR_ELT(new, n, x);
  UNPROTECT(1);
  return new;
}

SEXP realToSEXP (int n, double *arr){
    SEXP Rval;
    PROTECT(Rval = allocVector(REALSXP, n));
    for (int i = 0; i < n; i++)
        REAL(Rval)[i] = arr[i];
    UNPROTECT(1);
    return Rval;
}

SEXP intToSEXP (int n, int *arr){
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, n));
    for (int i = 0; i < n; i++)
        INTEGER(Rval)[i] = arr[i];
    UNPROTECT(1);
    return Rval;
}
