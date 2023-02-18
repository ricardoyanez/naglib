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

 S14AAF returns the value of the Gamma function.

 This wapper uses the GNU C Library function tgamma().

*/

double c_s14aaf_( double *x, int *ifail ) {
  feclearexcept(FE_ALL_EXCEPT);
  double y = tgamma(*x);
  /* check for function math errors */
  *ifail = 0;
  if ( fetestexcept(FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW|FE_UNDERFLOW) ) {
    if ( errno == ERANGE ) {
      fprintf(stderr,"c_s14aaf: ");
      if ( fetestexcept(FE_OVERFLOW) ) {
	*ifail = 1;
	fprintf(stderr,"argument too large and positive. ");
      }
      if ( fetestexcept(FE_UNDERFLOW) ) {
	*ifail = 2;
	fprintf(stderr,"argument too large and negative. ");
      }
      if ( fetestexcept(FE_DIVBYZERO) ) {
	*ifail = 3;
	fprintf(stderr,"argument too close to zero. ");
      }
      fprintf(stderr,"Range error when calling tgamma().\n");
    }
    else if ( errno == EDOM ) {
      *ifail = 4;
      fprintf(stderr,"c_s14aaf: argument is a negative integer. Domain error when calling tgamma().\n");
    }
  }
  return y;
}

