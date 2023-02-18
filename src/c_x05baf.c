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

 X05BAF returns the amount of processor time used since an unspecified 
 previous time, via the routine name.

 This wapper uses the GNU C Library function clock().

 Dividing clock() by CLOCKS_PER_SEC gives the processor time in seconds.

*/

double c_x05baf_() {
  return (double)clock()/CLOCKS_PER_SEC;
}
