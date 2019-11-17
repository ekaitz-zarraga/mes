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

struct scm *
deep_variable_ref (struct scm *var)
{
  assert_variable (1, var);
  struct scm *ref = var->variable;
  struct scm *value = ref->cdr;
  if (value == cell_undefined)
    error (cell_symbol_unbound_variable, var);
  if (value->type == TVARIABLE)
    value = value->variable;
  return value;
}

struct scm *
variable_ref (struct scm *var)
{
  assert_variable (1, var);
  struct scm *value = var->variable;
  if (value == cell_undefined)
    error (cell_symbol_unbound_variable, var);
  return value;
}

struct scm *
variable_set_x (struct scm *var, struct scm *value)
{
  assert_variable (1, var);
  var->variable = value;
  return cell_unspecified;
}

struct scm *
variable_bound_p (struct scm *var)
{
  assert_variable (1, var);
  struct scm *value = var->variable;
  if (value != cell_undefined)
    return cell_t;
  return cell_f;
}

struct scm *
handle_set_x (struct scm *handle, struct scm *value)
{
  struct scm *x = handle->cdr;
  if (x->type == TVARIABLE)
    x->variable = value;
  else
    handle->cdr = value;
  return cell_unspecified;
}

/*
  GUILE has `proc': scm_current_module -> scm_module_lookup_closure -> standard-eval-closure:

  BUT: define-p: module-make-local-var!, !define-p: module-variable

 */
struct scm *
lookup_handle (struct scm *name, struct scm *define_p)
{
  struct scm *handle = handle = assq (name, R0);

  if (handle == cell_f)
    {
      struct scm *module = current_module ();
      if (define_p == cell_f)
        {
          if (module == M0)
            handle = hashq_get_handle_ (M0, name);
          else
            handle = module_handle (module, name);
        }
      else
        {
          struct scm *table = module_defines (module);
          handle = hashq_get_handle_ (table, name);
          if (handle == cell_f)
            handle = hashq_set_handle_x (table, name, cell_f);
        }
    }

  return handle;
}

struct scm *
lookup_ref (struct scm *name, struct scm *bound_p)
{
  struct scm *handle = lookup_handle (name, cell_f);
  if (handle->type == TPAIR)
    return handle->cdr;
  if (bound_p == cell_t)
    error (cell_symbol_unbound_variable, name);
  return cell_undefined;
}

struct scm *
lookup_ref_ (char const *name)
{
  return lookup_ref (cstring_to_symbol (name), cell_f);
}