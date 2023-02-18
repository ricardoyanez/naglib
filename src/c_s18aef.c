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

 S18AEF returns the value of the modiﬁed Bessel Function of zeroth
 order I0(x). On overflow, function returns inf.

 This wapper uses the GSL Bessel functions.

*/

double c_s18aef_( double *x, int *ifail ) {
  gsl_sf_result result;
  gsl_set_error_handler_off();
  int status = gsl_sf_bessel_I0_e(*x,&result);
  *ifail = 0;
  if ( status ) {
    if ( GSL_ERANGE ) {
      fprintf(stderr,"c_s18aef: Range error in GSL.\n");
    }
    *ifail = 1;
  }
  return result.val;
}
