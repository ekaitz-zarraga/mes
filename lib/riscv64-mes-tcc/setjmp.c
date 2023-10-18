/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
 * Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
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

#include <setjmp.h>
#include <stdlib.h>

void
longjmp (jmp_buf env, int val)
{
    val = val == 0 ? 1 : val;
    asm(
    "ld s0, a0,0\n\t"
    "ld s1, a0,8\n\t"
    "ld s2, a0,16\n\t"
    "ld s3, a0,24\n\t"
    "ld s4, a0,32\n\t"
    "ld s5, a0,40\n\t"
    "ld s6, a0,48\n\t"
    "ld s7, a0,56\n\t"
    "ld s8, a0,64\n\t"
    "ld s9, a0,72\n\t"
    "ld s10,a0,80\n\t"
    "ld s11,a0,88\n\t"
    "ld sp, a0,96\n\t"
    "ld ra, a0,104\n\t"
#if HAVE_FLOAT_ASM && HAVE_FLOAT && ! __riscv_float_abi_soft
    "fld fs0, a0,112\n\t"
    "fld fs1, a0,120\n\t"
    "fld fs2, a0,128\n\t"
    "fld fs3, a0,136\n\t"
    "fld fs4, a0,144\n\t"
    "fld fs5, a0,152\n\t"
    "fld fs6, a0,160\n\t"
    "fld fs7, a0,168\n\t"
    "fld fs8, a0,176\n\t"
    "fld fs9, a0,184\n\t"
    "fld fs10,a0,192\n\t"
    "fld fs11,a0,200\n\t"
#endif
    );
}

int
setjmp (jmp_buf env)
{
    asm(
    "sd  a0, s0,0\n\t"
    "sd  a0, s1,8\n\t"
    "sd  a0, s2,16\n\t"
    "sd  a0, s3,24\n\t"
    "sd  a0, s4,32\n\t"
    "sd  a0, s5,40\n\t"
    "sd  a0, s6,48\n\t"
    "sd  a0, s7,56\n\t"
    "sd  a0, s8,64\n\t"
    "sd  a0, s9,72\n\t"
    "sd  a0, s10,80\n\t"
    "sd  a0, s11,88\n\t"
    "sd  a0, sp,96\n\t"
    "sd  a0, ra,104\n\t"
#if HAVE_FLOAT_ASM && HAVE_FLOAT && ! __riscv_float_abi_soft
    "fsd a0, fs0,112\n\t"
    "fsd a0, fs1,120\n\t"
    "fsd a0, fs2,128\n\t"
    "fsd a0, fs3,136\n\t"
    "fsd a0, fs4,144\n\t"
    "fsd a0, fs5,152\n\t"
    "fsd a0, fs6,160\n\t"
    "fsd a0, fs7,168\n\t"
    "fsd a0, fs8,176\n\t"
    "fsd a0, fs9,184\n\t"
    "fsd a0, fs10,192\n\t"
    "fsd a0, fs11,200\n\t"
#endif
    );
  return 0;
}

