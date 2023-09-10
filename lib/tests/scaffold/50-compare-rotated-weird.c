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

#include <stdint.h>
#include <mes/lib.h>
#include <string.h>

int
main ()
{
  uint64_t val = 0x27c1c;
  uint64_t addr = 0x28bb4;
  uint64_t off64 = (int64_t)((int64_t)(val - addr + 0x800) >> 12);
  uint64_t cond = (off64 + ((uint64_t)1 << 20)) >> 21;
  if (cond)
    {
      eputs ("failure: ");
      eputs (itoa (cond));
      eputs ("\n");
      return 2;
    }
  return 0;
}
