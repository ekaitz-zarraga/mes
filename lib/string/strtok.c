/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2022 Janneke Nieuwenhuizen <janneke@gnu.org>
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

#include <string.h>

char *
strtok (char *new_string, char const *delimiters)
{
  static char *mark;
  if (new_string)
    mark = new_string;
  if (!*mark)
    return 0;
  while (strchr (delimiters, *mark))
    mark++;
  char *point = mark;
  while (*mark && !strchr (delimiters, *mark))
    mark++;
  if (*mark)
    {
      *mark = 0;
      mark++;
    }
  return point;
}
