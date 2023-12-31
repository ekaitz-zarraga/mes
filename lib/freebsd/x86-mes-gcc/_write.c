/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2019,2020,2023 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib-mini.h"

#define SYS_write   "0x04"

// *INDENT-OFF*
ssize_t
_write (int filedes, void const *buffer, size_t size)
{
  long r;
  asm (
       "mov    $"SYS_write",%%eax\n\t"
       "mov    %3,%%edx\n\t"
       "push   %%edx\n\t"
       "mov    %2,%%ecx\n\t"
       "push   %%ecx\n\t"
       "mov    %1,%%ebx\n\t"
       "push   %%ebx\n\t"
       "push   %%ebx\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (filedes), "rm" (buffer), "rm" (size)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
}
