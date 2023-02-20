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

 This function is a C port of the FORTRAN subroutine I1MACH. The original
 FORTRAN version of I1MACH can be found in Netlib (https://netlib.org/).

*/

long c_i1mach_( int *i ) {
  switch ( *i ) {
  case 1:
    return 5;    /* standard input */
  case 2:
    return 6;    /* standard output */
  case 3:
    return 7;    /* standard punch */
  case 4:
    return 0;    /* standard error */
  case 5:
    return 32;   /* bits per integer */
  case 6:
    return sizeof(int);
  case 7:
    return 2;    /* base for integers */
  case 8:
    return 31;   /* digits of integer base */
  case 9:
    return LONG_MAX;
  case 10:
    return FLT_RADIX;
  case 11:
    return FLT_MANT_DIG;
  case 12:
    return FLT_MIN_EXP;
  case 13:
    return FLT_MAX_EXP;
  case 14:
    return DBL_MANT_DIG;
  case 15:
    return DBL_MIN_EXP;
  case 16:
    return DBL_MAX_EXP;
  default:
    fprintf(stderr,"Invalid argument: i1mach(%d)\n",*i);
    exit(EXIT_FAILURE);
  }
  return EXIT_SUCCESS;
}
