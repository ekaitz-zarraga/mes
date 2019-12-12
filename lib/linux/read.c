/* -*-comment-start: "//";comment-end:""-*-
 * GNU MES --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <linux/syscall.h>
#include <syscall.h>
#include <mes/lib.h>
#include <fcntl.h>

ssize_t
read (int filedes, void *buffer, size_t size)
{
  ssize_t bytes = _sys_call3 (SYS_read, (int) filedes, (long) buffer, (long) size);
  if (__mes_debug () > 4)
    {
      if (bytes == 1)
        {
          eputs ("read fd=");
          eputs (itoa ((int) filedes));
          eputs (" c=");
          eputc (*(char *) buffer);
          eputs ("\n");
        }
      else
        {
          eputs ("read fd=");
          eputs (itoa ((int) filedes));
          eputs (" bytes=");
          eputs (itoa (bytes));
          eputs ("\n");
        }
    }
  return bytes;
}
