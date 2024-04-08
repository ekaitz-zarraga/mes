/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>
#include <stdio.h>
#include <string.h>

int
main ()
{
  char buf_abc[20];
  char buf_xxx[20];
  char buf_0_9[20];
  int i = sscanf ("@abcxxx x0", "@%5[a-bc]%s%[^0-9]", &buf_abc, &buf_xxx, &buf_0_9);
  if (i != 3)
    return 1;
  eputs ("buf_abc=");
  eputs (buf_abc);
  eputs ("\n");
  eputs ("buf_xxx=");
  eputs (buf_xxx);
  eputs ("\n");
  eputs ("buf_0_9=");
  eputs (buf_0_9);
  eputs ("\n");
  if (strcmp (buf_abc, "abc"))
    return 2;
  if (strcmp (buf_xxx, "xxx"))
    return 3;
  if (strcmp (buf_0_9, " x"))
    return 4;
  return 0;
}
