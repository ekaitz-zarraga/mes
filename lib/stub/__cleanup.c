/* -*-comment-start: "//";comment-end:""-*-
 * GNU MES --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU MES.
 *
 * GNU MES is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU MES is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU MES.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <mes/lib.h>

int
__cleanup ()
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__cleanup stub\n");
  stub = 1;
  return 0;
}
