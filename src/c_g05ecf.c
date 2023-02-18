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

 G05CEF sets up the reference vector rv of dimension nr for a Poisson 
 distribution with mean mu.

 This wapper uses the GSL Random Number Distribution functions.

*/

void c_g05ecf_( double *mu, double *rv, int *nr ) {
  for ( int i = 0 ; i < *nr ; i++ ) {
    rv[i] = gsl_cdf_poisson_P(i,*mu);
  }
  return;
}
