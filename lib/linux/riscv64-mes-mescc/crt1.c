/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2023 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib-mini.h"
#include "linux/riscv64/syscall.h"

int main (int argc, char *argv[], char *envp[]);

/* mesc will generate the following preamble:

   push    ra
   push    fp
*/
int
_start ()
{
  asm ("rd_t1 rs1_fp mv");
  asm ("rd_t1 rs1_t1 !0x18 addi"); // 0x10 to skip over pushed fp+ra, 0x8 to skip over argc
  asm ("rd_t5 rs1_fp !0x10 addi"); // 0x10 to skip over pushed fp+ra
  asm ("rd_t0 rs1_t5 ld");
  asm ("rd_t0 rs1_t0 !1 addi");
  asm ("rd_t5 !3 addi"); // skip over all arguments and the final NULL
  asm ("rd_t0 rs1_t0 rs2_t5 sll");
  asm ("rd_t0 rs1_t0 rs2_t1 add");
  asm ("rd_sp rs1_sp !-8 addi"); // push envp onto stack
  asm ("rs1_sp rs2_t0 sd");
  asm ("rd_sp rs1_sp !-8 addi"); // push argv onto stack
  asm ("rs1_sp rs2_t1 sd");
  asm ("rd_t5 rs1_fp !0x10 addi"); // 0x10 to skip over pushed fp+ra
  asm ("rd_t0 rs1_t5 ld");
  asm ("rd_sp rs1_sp !-8 addi"); // push argc onto stack
  asm ("rs1_sp rs2_t0 sd");

  __init_io ();
  main ();

  asm ("rd_a0 rs1_t0 mv");
  asm (RISCV_SYSCALL(SYS_exit));
  asm ("ecall");
  asm ("ebreak");
}
