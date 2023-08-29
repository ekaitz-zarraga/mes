/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include<stdint.h>
#include<stdio.h>

int
main ()
{
  uint32_t r = 0xFFFFFFFF;
  r = r <<30 >>30;          // higher 30 bits are cleared because they leave
                            // the register to the left
  if (r != 3)
    return 1;

  uint16_t s = 0xFFFF;
  s = s <<14 >>14;          // s<<14 is promoted to int, the higher bits are
                            // not cleared, because there's space in the
                            // register
  if (s != 0xFFFF)
    return 2;

  return 0;
}
