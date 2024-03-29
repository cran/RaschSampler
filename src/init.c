#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(sampler)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(unpack)(void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"sampler", (DL_FUNC) &F77_NAME(sampler), 10},
    {"unpack",  (DL_FUNC) &F77_NAME(unpack),   5},
    {NULL, NULL, 0}
};

void R_init_RaschSampler(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
