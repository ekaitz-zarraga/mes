/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_STDARG_H
#define __MES_STDARG_H 1

#if WITH_GLIBC
#undef __MES_STDARG_H
#include_next <stdarg.h>
#else // ! WITH_GLIBC

#include <sys/types.h>

#if __GNUC__ && __x86_64__
#define __FOO_VARARGS 1
#endif

typedef char *va_list;
#define va_start(ap, last) (void)((ap) = (char*)(&(last) + 1))
#define va_arg(ap, type) (type)(((long*)((ap) = ((ap) + sizeof (void*))))[-1])
#define va_end(ap) (void)((ap) = 0)
#define va_copy(dest, src) dest = src

int vfprintf (FILE * stream, char const *template, va_list ap);
int vprintf (char const *format, va_list ap);
int vsprintf (char *str, char const *format, va_list ap);
int vsnprintf (char *str, size_t size, char const *format, va_list ap);
int vsscanf (char const *s, char const *template, va_list ap);

#endif // ! WITH_GLIBC

#endif // __MES_STDARG_H
