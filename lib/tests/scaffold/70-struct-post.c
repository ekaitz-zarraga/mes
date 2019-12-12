/* -*-comment-start: "//";comment-end:""-*-
 * GNU MES --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

struct foo
{
  int bar;
};

int
main ()
{
  struct foo f;
  struct foo *p = &f;
  //eputs ("p-f="); eputs (itoa (p-&f)); eputs ("\n");
  int i = 22;
  *p++ = f;
  //eputs ("p-f="); eputs (itoa (p-&f)); eputs ("\n");
  if (p - &f != 1)
    return 1;
  return 0;
}
