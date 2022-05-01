/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib.h"
#include "mes/mes.h"

SCM
apply_builtin0 (SCM fn)
{
  SCM (*fp) (void) = (function0_t) builtin_function (fn);
  return fp ();
}

SCM
apply_builtin1 (SCM fn, SCM x)
{
  SCM (*fp) (SCM) = (function1_t) builtin_function (fn);
  return fp (x);
}

SCM
apply_builtin2 (SCM fn, SCM x, SCM y)
{
  SCM (*fp) (SCM, SCM) = (function2_t) builtin_function (fn);
  return fp (x, y);
}

SCM
apply_builtin3 (SCM fn, SCM x, SCM y, SCM z)
{
  SCM (*fp) (SCM, SCM, SCM) = (function3_t) builtin_function (fn);
  return fp (x, y, z);
}
