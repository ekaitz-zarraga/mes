/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
 * Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
 * Copyright © 2024 Andrius Štikonas <andrius@stikonas.eu>
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

asm (".global __longjmp\n\t"
     ".global _longjmp\n\t"
     ".global longjmp\n\t"
     ".type __longjmp, %function\n\t"
     ".type _longjmp,  %function\n\t"
     ".type longjmp,   %function\n\t"
     "__longjmp:\n\t"
     "_longjmp:\n\t"
     "longjmp:\n\t"
     "ld s0,    0(a0)\n\t"
     "ld s1,    8(a0)\n\t"
     "ld s2,    16(a0)\n\t"
     "ld s3,    24(a0)\n\t"
     "ld s4,    32(a0)\n\t"
     "ld s5,    40(a0)\n\t"
     "ld s6,    48(a0)\n\t"
     "ld s7,    56(a0)\n\t"
     "ld s8,    64(a0)\n\t"
     "ld s9,    72(a0)\n\t"
     "ld s10,   80(a0)\n\t"
     "ld s11,   88(a0)\n\t"
     "ld sp,    96(a0)\n\t"
     "ld ra,    104(a0)\n\t"
#if HAVE_FLOAT_ASM && HAVE_FLOAT && ! __riscv_float_abi_soft
     "fld fs0,  112(a0)\n\t"
     "fld fs1,  120(a0)\n\t"
     "fld fs2,  128(a0)\n\t"
     "fld fs3,  136(a0)\n\t"
     "fld fs4,  144(a0)\n\t"
     "fld fs5,  152(a0)\n\t"
     "fld fs6,  160(a0)\n\t"
     "fld fs7,  168(a0)\n\t"
     "fld fs8,  176(a0)\n\t"
     "fld fs9,  184(a0)\n\t"
     "fld fs10, 192(a0)\n\t"
     "fld fs11, 200(a0)\n\t"
#endif
     "seqz a0, a1\n\t"
     "add a0, a0, a1\n\t"
     "ret\n\t");

asm (".global __setjmp\n\t"
     ".global _setjmp \n\t"
     ".global setjmp\n\t"
     ".type __setjmp, %function\n\t"
     ".type _setjmp,  %function\n\t"
     ".type setjmp,   %function\n\t"
     "__setjmp:\n\t"
     "_setjmp:\n\t"
     "setjmp:\n\t"
     "sd s0,    0(a0)\n\t"
     "sd s1,    8(a0)\n\t"
     "sd s2,    16(a0)\n\t"
     "sd s3,    24(a0)\n\t"
     "sd s4,    32(a0)\n\t"
     "sd s5,    40(a0)\n\t"
     "sd s6,    48(a0)\n\t"
     "sd s7,    56(a0)\n\t"
     "sd s8,    64(a0)\n\t"
     "sd s9,    72(a0)\n\t"
     "sd s10,   80(a0)\n\t"
     "sd s11,   88(a0)\n\t"
     "sd sp,    96(a0)\n\t"
     "sd ra,    104(a0)\n\t"
#if HAVE_FLOAT_ASM && HAVE_FLOAT && ! __riscv_float_abi_soft
     "fsd fs0,  112(a0)\n\t"
     "fsd fs1,  120(a0)\n\t"
     "fsd fs2,  128(a0)\n\t"
     "fsd fs3,  136(a0)\n\t"
     "fsd fs4,  144(a0)\n\t"
     "fsd fs5,  152(a0)\n\t"
     "fsd fs6,  160(a0)\n\t"
     "fsd fs7,  168(a0)\n\t"
     "fsd fs8,  176(a0)\n\t"
     "fsd fs9,  184(a0)\n\t"
     "fsd fs10, 192(a0)\n\t"
     "fsd fs11, 200(a0)\n\t"
#endif
     "li a0, 0\n\t"
     "ret\n\t");
