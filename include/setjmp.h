/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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
#ifndef __MES_SETJMP_H
#define __MES_SETJMP_H 1

#if SYSTEM_LIBC
#undef __MES_SETJMP_H
#include_next <setjmp.h>
#else // ! SYSTEM_LIBC

#if __arm__
#if __GNUC__ || __TINYC__
#warning "It is not supported to use mes' setjmp implementation together with GCC.  Continuing with best-effort implementation."
typedef struct
{
  long __sp;
  long __lr;
  long __registers[8]; /* Note: Keep in sync with lib/arm-mes-gcc/setjmp.c */
} __jmp_buf;
#else
typedef struct
{
  long __fp;
  long __lr;
  long __sp;
} __jmp_buf;
#endif
#elif __riscv && (__GNUC__ || __TINYC__)
typedef struct
{
  long __sp;
  long __lr;
  long __registers[14]; /* Note: Keep in sync with lib/riscv64-mes-gcc/setjmp.c */
} __jmp_buf;
#else
typedef struct
{
  long __bp;
  long __pc;
  long __sp;
} __jmp_buf;
#endif
typedef __jmp_buf jmp_buf[1];

void longjmp (jmp_buf env, int val);
int setjmp (jmp_buf env);

#endif // ! SYSTEM_LIBC

#endif // __MES_SETJMP_H
