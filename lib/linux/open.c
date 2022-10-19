/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2022 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <fcntl.h>
#include <stdarg.h>

#if __M2__
int
open (char *file_name, int flags, int mask)
{
  int r = _sys_call3 (SYS_open, file_name, flags, mask);
  if (r > 2)
    __ungetc_clear (r);
  return r;
}
#else // !__M2__
int
open (char const *file_name, int flags, ...)
{
  va_list ap;
  va_start (ap, flags);
  int mask = va_arg (ap, int);
  int r = _sys_call3 (SYS_open, (long) file_name, flags, mask);
  va_end (ap);
  if (r > 2)
    __ungetc_clear (r);
  return r;
}
#endif // __M2__