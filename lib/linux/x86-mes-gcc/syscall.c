/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <errno.h>
#include <linux/x86/syscall.h>

// *INDENT-OFF*
long
__sys_call (long sys_call)
{
  long r;
  asm (
       "mov    %1,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call)
       : "eax"
       );
  return r;
}

long
__sys_call1 (long sys_call, long one)
{
  long r;
  asm (
       "mov    %1,%%eax\n\t"
       "mov    %2,%%ebx\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one)
       : "eax", "ebx"
       );
  return r;
}

long
__sys_call2 (long sys_call, long one, long two)
{
  long r;
  asm (
       "mov    %1,%%eax\n\t"
       "mov    %2,%%ebx\n\t"
       "mov    %3,%%ecx\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one), "rm" (two)
       : "eax", "ebx", "ecx"
       );
  return r;
}

long
__sys_call3 (long sys_call, long one, long two, long three)
{
  long r;
  asm (
       "mov    %2,%%ebx\n\t"
       "mov    %3,%%ecx\n\t"
       "mov    %4,%%edx\n\t"
       "mov    %1,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one), "rm" (two), "rm" (three)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
}

long
__sys_call4 (long sys_call, long one, long two, long three, long four)
{
  long r;
  asm (
       "mov    %2,%%ebx\n\t"
       "mov    %3,%%ecx\n\t"
       "mov    %4,%%edx\n\t"
       "mov    %5,%%esi\n\t"
       "mov    %1,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one), "rm" (two), "rm" (three), "rm" (four)
       : "eax", "ebx", "ecx", "edx", "esi"
       );
  return r;
}

long
__sys_call6 (long sys_call, long one, long two, long three, long four, long five, long six)
{
  long r;
  asm (
       "mov    %1,%%eax\n\t"
       "mov    %%ebp,%%ebx\n\t"
       "add    $0x0c,%%ebx\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call)
       : "eax", "ebx"
       );
  return r;
}
// *INDENT-ON*

long
_sys_call (long sys_call)
{
  long r = __sys_call (sys_call);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call1 (long sys_call, long one)
{
  long r = __sys_call1 (sys_call, one);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call2 (long sys_call, long one, long two)
{
  long r = __sys_call2 (sys_call, one, two);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call3 (long sys_call, long one, long two, long three)
{
  long r = __sys_call3 (sys_call, one, two, three);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call4 (long sys_call, long one, long two, long three, long four)
{
  long r = __sys_call4 (sys_call, one, two, three, four);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call6 (long sys_call, long one, long two, long three, long four, long five, long six)
{
  long r = __sys_call6 (sys_call, one, two, three, four, five, six);
#if 0
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
#endif
  return r;
}
