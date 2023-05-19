/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
 * Copyright © 2023 Andrius Štikonas <andrius@stikonas.eu>
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
#include <linux/riscv64/syscall.h>

static long
__sys_call_internal (long sys_call)
{
  asm ("rd_a7 rs1_fp !16 ld");

  asm ("ecall");

  asm ("rd_t0 rs1_a0 mv");
}

static long
__sys_call2_internal (long sys_call, long one, long two)
{
  asm ("rd_a7 rs1_fp !16 ld");
  asm ("rd_a0 rs1_fp !24 ld");
  asm ("rd_a1 rs1_fp !32 ld");

  asm ("ecall");

  asm ("rd_t0 rs1_a0 mv");
}

/* Return < 0 on error (errno-like value from kernel), or 0 on success */
int
__raise (int signum)
{
  long pid = __sys_call_internal (SYS_getpid);
  if (pid < 0)
    return pid;
  else
    return __sys_call2_internal (SYS_kill, pid, signum);
}
