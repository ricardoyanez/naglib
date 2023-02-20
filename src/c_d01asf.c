/*

 Copyright (c) 2022-2023 Ricardo Yanez <ricardo.yanez@calel.org>

 This program is free software: you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation, either version 3 of the License, or (at your option) any later
 version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 more details.

 You should have received a copy of the GNU General Public License along with
 this program. If not, see <https://www.gnu.org/licenses/>. 

*/

#include "cnag_gsl.h"

/*

 This function "casts" the external Fortran function to a GSL function.

*/

f_user_function *f_d01asf;

double g_d01asf(double x, void *params ) {
  return f_d01asf(&x,params);
}

/*

 D01ASF calculates an approximation to the integral of a function f(x)
 over an infinite or semi-infinite interval.

 This wapper uses GSL Numerical Integration functions.

*/

void c_d01asf_( f_user_function *f, double *a, double *omega, int *key, double *epsabs,
		double *result, double *abserr, int *limlst, int *lst, double *erlst,
		double *rslst, int *ierlst, double *w, int *lw, int *iw, int *liw,
		int *ifail ) {

  int status;
  gsl_integration_workspace *ws = gsl_integration_workspace_alloc(1000);
  gsl_integration_workspace *wc = gsl_integration_workspace_alloc(1000);
  gsl_integration_qawo_table *wf;
  
  if ( *key == 1 ) {
    wf = gsl_integration_qawo_table_alloc(*omega,1.0,GSL_INTEG_COSINE,1000);
  }
  else if ( *key == 2 ) {
    wf = gsl_integration_qawo_table_alloc(*omega,1.0,GSL_INTEG_SINE,1000);
  }
  else {
    fprintf(stderr,"c_d01asf: invalid integral key.\n");
    exit(EXIT_FAILURE);
  }

  gsl_function F;

  f_d01asf = f;
  F.function = g_d01asf;
  F.params = 0;

  status = gsl_integration_qawf(&F,*a,*epsabs,ws->limit,ws,wc,wf,result,abserr);

  if ( status == GSL_SUCCESS ) {
    *ifail = 0;
  }
  else if ( status == GSL_EMAXITER ) {
    *ifail = 1;
  }
  else if ( status == GSL_EROUND ) {
    *ifail = 2;
  }
  else if ( status == GSL_ESING ) {
    *ifail = 3;
  }
  else if ( status == GSL_EDIVERGE ) {
    *ifail = 5;
  }
  else if ( status == GSL_EDOM ) {
    *ifail = 6;
  }
  else {
    *ifail = 4;
  }

  for ( int i = 0 ; i < ws->size ; i++ ) {
    erlst[i] = ws->elist[i];
    rslst[i] = ws->rlist[i];
  }
  
  /* number of intervals used */
  *lst = ws->size;

  gsl_integration_workspace_free(ws);
  gsl_integration_workspace_free(wc);
  gsl_integration_qawo_table_free(wf);

  return;
}
