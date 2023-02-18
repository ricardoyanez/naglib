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

 S15ADF returns the value of the complementary error function erfc x.

 This wapper uses the GNU C Library function erfc().

*/

double c_s15adf_( double *x, int *ifail ) {
  feclearexcept(FE_ALL_EXCEPT);
  double f = erfc(*x);
  *ifail = 0;
  /* check for function math errors */
  if ( fetestexcept(FE_UNDERFLOW) ) {
    fprintf(stderr,"c_s15adf: argument too large and negative. Error when calling erfc().\n");
    *ifail = 1;
  }
  return f;
}
