/*
 * Copyright (c) 2022-2023 Ricardo Yanez <ricardo.yanez@calel.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 */

#include "cnag.h"

/*
 * G05DDF returns a pseudo-random real number taken from a Normal 
 * (Gaussian) distribution with mean a and standard deviation b.
 *
 * This wapper uses the GSL Random Number Distribution functions.
 *
 */

double c_g05ddf_( double *a, double *b ) {
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
  return gsl_ran_gaussian(r,*b) + *a;
}

/*
 * G05CEF sets up the reference vector rv of dimension nr for a Poisson 
 * distribution with mean mu.
 *
 * This wapper uses the GSL Random Number Distribution functions.
 *
 */

void c_g05ecf_( double *mu, double *rv, int *nr ) {
  for ( int i = 0 ; i < *nr ; i++ ) {
    rv[i] = gsl_cdf_poisson_P(i,*mu);
  }
  return;
}

/*
 * G05EYF returns a pseudo-random integer taken from a discrete distribution 
 * defined by a reference vector.
 *
 * This wapper uses the GSL Random Number Generation functions.
 *
 */

int c_g05eyf_( double *rv, int *nr ) {
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
  double s = gsl_rng_uniform (r);
  int i = 0;
  while ( 1 )  {
    if ( rv[i] > s ) break;
    i++;
  }
  return i;
}

/*
 * S14AAF returns the value of the Gamma function.
 *
 * This wapper uses the GNU C Library function tgamma().
 *
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

/*
 * S14ABF returns a value for the logarithm of the Gamma function, ln gamma(x),
 * via the routine name.
 *
 * This wapper uses the GNU C Library function lgamma().
 *
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

/*
 * S15ADF returns the value of the complementary error function erfc x.
 *
 * This wapper uses the GNU C Library function erfc().
 *
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

/*
 * S18AEF returns the value of the modiﬁed Bessel Function of zeroth
 * order I0(x). On overflow, function returns inf.
 *
 * This wapper uses the GSL Bessel functions.
 *
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

/*
 * S18AFF returns the value of the modiﬁed Bessel Function of first
 * order I1(x). On overflow, function returns inf.
 *
 * This wapper uses the GSL Bessel functions.
 *
 */

double c_s18aff_( double *x, int *ifail ) {
  gsl_sf_result result;
  gsl_set_error_handler_off();
  int status = gsl_sf_bessel_I1_e(*x,&result);
  *ifail = 0;
  if ( status ) {
    if ( GSL_ERANGE ) {
      fprintf(stderr,"c_s18aff: Range error in GSL.\n");
    }
    *ifail = 1;
  }
  return result.val;
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

double c_d1mach_( int *i ) {
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
