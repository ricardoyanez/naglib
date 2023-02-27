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

#include "cnag_gsl_rng.h"

/*

 G05EYF returns a pseudo-random integer taken from a discrete distribution 
 defined by a reference vector.

 This wapper uses GSL Random Number Generation functions.

*/

int c_g05eyf_( double *rv, int *nr ) {
  const gsl_rng_type *T;
 /* initialize the GSL random number generator once */
  if ( gsl_rng_init ) {
    srand(time(NULL));
    long seed = rand();
    /* convert seed to a string */
    char cseed[256];
    sprintf(cseed,"%ld",seed);
    if ( setenv("GSL_RNG_SEED",cseed,1) ) {
      fprintf(stderr,"c_g05eyf: error setting GSL_RNG_SEED\n");
      exit(EXIT_FAILURE);
    }
    gsl_rng_env_setup();
    T = gsl_rng_default;
    r = gsl_rng_alloc(T);
    gsl_rng_init = false;
  }
  double s = gsl_rng_uniform(r);
  int i = 0;
  while ( 1 )  {
    if ( rv[i] > s ) break;
    i++;
  }
  return i;
}
