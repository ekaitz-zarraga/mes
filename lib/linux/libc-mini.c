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

#if __MESC__ && __i386__
#include <linux/x86-mes/mini.c>
#elif __MESC__ && __x86_64__
#include <linux/x86_64-mes/mini.c>
#elif __i386__
#include <linux/x86-mes-gcc/mini.c>
#elif __x86_64__
#include <linux/x86_64-mes-gcc/mini.c>
#else
#error arch not supported
#endif

#include <posix/write.c>
