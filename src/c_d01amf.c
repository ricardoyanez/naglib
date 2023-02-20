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

f_user_function *f_d01amf;

double g_d01amf(double x, void *params ) {
  return f_d01amf(&x,params);
}

/*

 D01AMF calculates an approximation to the integral of a function f(x)
 over an infinite or semi-infinite interval.

 This wapper uses GSL Numerical Integration functions.

*/

void c_d01amf_( f_user_function *f, double *bound, int *inf, double *epsabs,
	       double *epsrel, double *result, double *abserr, double *w,
	       int *lw, int *iw, int *liw, int *ifail ) {

  int status;
  gsl_integration_workspace *ws = gsl_integration_workspace_alloc(1000);
  gsl_function F;

  f_d01amf = f;
  F.function = g_d01amf;
  F.params = 0;

  if ( *inf == 1 ) {
    status = gsl_integration_qagiu(&F,*bound,*epsabs,*epsrel,ws->limit,ws,result,abserr);
  }
  else if ( *inf == -1 ) {
    status = gsl_integration_qagil(&F,*bound,*epsabs,*epsrel,ws->limit,ws,result,abserr);
  }
  else if ( *inf == 2 ) {
    status = gsl_integration_qagi(&F,*epsabs,*epsrel,ws->limit,ws,result,abserr);
  }
  else {
    fprintf(stderr,"c_d01amf: invalid integration range.\n");
    exit(EXIT_FAILURE);
  }

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

  /* number of intevals used */
  iw[0] = ws->size;

  gsl_integration_workspace_free(ws);

  return;
}
