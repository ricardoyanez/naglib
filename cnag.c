/*
 * Copyright (c) 2022-2023 Ricardo Yanez <ricardo.yanez@calel.org>
 *
 * C wrappers to the NAG Fortran Library for GRAZING
 *
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <math.h>
#include <fenv.h>
#include <float.h>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

/*
 * Random samling of a Gaussian (Normal) distribution with mean being the mean
 * and sigma the standard deviation of the distribution.
 *
 * (GSL version)
 *
 */

double gaussd_gsl( double mean, double sigma ) {
  static bool gsl_rng_init = true;
  static gsl_rng_type *T;
  static gsl_rng *r;
  /* initialize the GSL random number generator once */
  if ( gsl_rng_init ) {
    srand(time(NULL));
    long seed = rand();
    /* convert seed to a string */
    char cseed[256];
    sprintf(cseed,"%ld",seed);
    if ( setenv("GSL_RNG_SEED",cseed,1) ) {
      fprintf(stderr,"error setting GSL_RNG_SEED\n");
      exit(EXIT_FAILURE);
    }
    gsl_rng_env_setup();
    T = gsl_rng_default;
    r = gsl_rng_alloc(T);
    gsl_rng_init = false;
  }
  return gsl_ran_gaussian(r,sigma) + mean;
}

/*
 * G05DDF returns a pseudo-random real number taken from a Normal 
 * (Gaussian) distribution with mean a and standard deviation b.
 */

double c_g05ddf_( double *a, double *b ) {
  double x = gaussd_gsl(*a,*b);
  return gaussd_gsl(*a,*b);
}

/*
 * S14AAF returns the value of the Gamma function, via the routine name
 *
 * This wapper uses the GNU C Library function tgamma().
 *
 */

double c_s14aaf_(double *x, int *ifail) {

  double f;
  feclearexcept(FE_ALL_EXCEPT);
  f = tgamma(*x);
  *ifail = 0;
  if ( fetestexcept(FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW|FE_UNDERFLOW) ) {
    *ifail = 1;
  }
  return f;
}

/*
 * S14ABF returns a value for the logarithm of the Gamma function, ln gamma(x),
 * via the routine name.
 *
 * This wapper uses the GNU C Library function lgamma().
 *
 */

double c_s14abf_(double *x, int *ifail) {

  double f;
  feclearexcept(FE_ALL_EXCEPT);
  f = lgamma(*x);
  *ifail = 0;
  if ( fetestexcept(FE_DIVBYZERO|FE_OVERFLOW) ) {
    *ifail = 1;
  }
  return f;
}

/*
 * S15ADF returns the value of the complementary error function, erfc x,
 * via the routine name.
 *
 * This wapper uses the GNU C Library function erfc().
 *
 */

double c_s15adf_(double *x, int *ifail) {

  double f;
  feclearexcept(FE_ALL_EXCEPT);
  f = erfc(*x);
  *ifail = 0;
  if ( fetestexcept(FE_UNDERFLOW) ) {
    *ifail = 1;
  }
  return f;
}

/*
 * X05BAF returns the amount of processor time used since an unspecified 
 * previous time, via the routine name.
 *
 * This wapper uses the GNU C Library function clock(). 
 * Dividing clock() by CLOCKS_PER_SEC gives the processor time in seconds.
 *
 */

double c_x05baf_() {
  return (double)clock()/CLOCKS_PER_SEC;
}

/*
 * This function is a C port of the FORTRAN subroutine D1MACH used by RKSUITE,
 * QUADPACK and AMOS. The original FORTRAN version of D1MACH can be found in
 * Netlib.
 *
 */

double c_d1mach_(int *i) {
  switch (*i) {
  case 1:
    return DBL_MIN;
  case 2:
    return DBL_MAX;
  case 3:
    return DBL_EPSILON/(double)FLT_RADIX;
  case 4:
    return DBL_EPSILON;
  case 5:
    return log10((double)FLT_RADIX);
  default:
    fprintf(stderr,"Invalid argument: d1mach(%ld)\n",*i);
    exit(EXIT_FAILURE);
  }
  return EXIT_SUCCESS;
}