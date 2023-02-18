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

#include "cnag.h"

/* This function "casts" the external Fortran function
 * to a GSL function.
 */ 

double g_c05adf(double x, void *params ) {
  return f_function(&x,params);
}

/*
 * NAG description: C05ADF locates a zero of a continuous function in a
 * given interval by combination of the methods of linear interpolation,
 * extrapolation and bisection.
 *
 * This wrapper uses GSL root-finding functions (Brent's method)
 *
 */

void c_c05adf_( double *a, double *b, double *eps, double *eta, f_user_function *f,
		double *x, int *ifail ) {

  int status;
  int iter = 0, max_iter = 100;
  const gsl_root_fsolver_type *T;
  gsl_root_fsolver *s;
  gsl_function F;

  f_function = f;
  F.function = g_c05adf;
  F.params = 0;

  T = gsl_root_fsolver_brent;
  s = gsl_root_fsolver_alloc(T);
  gsl_root_fsolver_set(s,&F,*a,*b);

  /* iterate for convergence */
  do {
    iter++;
    status = gsl_root_fsolver_iterate(s);
    *x = gsl_root_fsolver_root(s);
    *a = gsl_root_fsolver_x_lower(s);
    *b = gsl_root_fsolver_x_upper(s);
    status = gsl_root_test_interval(*a,*b,0.,*eps);
  } while ( status == GSL_CONTINUE && iter < max_iter );
  
  if ( status == GSL_SUCCESS ) {
    *ifail = 0;
  }
  else {
    if ( iter ==  max_iter ) {
      fprintf(stderr,"c_c05adf: convergence not achieve after %d iterations %d.\n",max_iter,iter);
    }
    *x = 0.;
    *ifail = 1;
  }

  /* free the solver */
  gsl_root_fsolver_free(s);

  return;
}
