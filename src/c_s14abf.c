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

#include "cnag_glib.h"

/*

 S14ABF returns a value for the logarithm of the Gamma function, ln gamma(x),
 via the routine name.
 
 This wapper uses the GNU C Library function lgamma().

*/

double c_s14abf_( double *x, int *ifail ) {
  feclearexcept(FE_ALL_EXCEPT);
  double f = lgamma(*x);
  *ifail = 0;
  /* check for function math errors */
  if ( fetestexcept(FE_DIVBYZERO) ) {
    fprintf(stderr,"c_s14abf: argument is a non-positive integer. Range error when calling lgamma().\n");
    *ifail = 1;
  }
  if ( fetestexcept(FE_OVERFLOW) ) {
    fprintf(stderr,"c_s14abf: argument too large and positive. Range error when calling lgamma().\n");
    *ifail = 2;
  }
  return f;
}
