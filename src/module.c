/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

SCM struct_ref_ (SCM x, long i);
SCM struct_set_x_ (SCM x, long i, SCM e);

SCM
make_module_type () ///(internal))
{
  SCM record_type_name = cstring_to_symbol ("<record-type>");
  SCM record_type = record_type_name; // FIXME
  SCM module_type_name = cstring_to_symbol ("<module>");
  SCM fields = cell_nil;
  fields = cons (cstring_to_symbol ("globals"), fields);
  fields = cons (cstring_to_symbol ("locals"), fields);
  fields = cons (cstring_to_symbol ("name"), fields);
  fields = cons (fields, cell_nil);
  fields = cons (module_type_name, fields);
  return make_struct (record_type, fields, cell_unspecified);
}

SCM
make_initial_module (SCM a) ///((internal))
{
  SCM module_type_name = cstring_to_symbol ("<module>");
  // SCM module_type = module_type_name; //FIXME
  SCM module_type = make_module_type ();
  a = acons (module_type_name, module_type, a);

  SCM hashq_type = make_hashq_type ();
  SCM hashq_type_name = cstring_to_symbol ("<hashq-table>");
  a = acons (hashq_type_name, hashq_type, a);

  SCM name = cons (cstring_to_symbol ("boot"), cell_nil);
  SCM globals = make_hash_table_ (0);
  SCM locals = cell_nil;

  SCM values = cell_nil;
  values = cons (globals, values);
  values = cons (locals, values);
  values = cons (name, values);
  values = cons (module_type_name, values);
  SCM module = make_struct (module_type, values, cell_module_printer);
  r0 = cell_nil;
  r0 = cons (CADR (a), r0);
  r0 = cons (CAR (a), r0);
  m0 = module;
  while (TYPE (a) == TPAIR)
    {
      if (g_debug > 3)
        {
          eputs ("entry="); display_error_ (CAR (a)); eputs ("\n");
        }
      module_define_x (module, CAAR (a), CDAR (a));
      a = CDR (a);
    }

  return module;
}

SCM
module_printer (SCM module)
{
  //module = m0;
  fdputs ("#<", g_stdout); display_ (struct_ref_ (module, 2)); fdputc (' ', g_stdout);
  fdputs ("name: ", g_stdout); display_ (struct_ref_ (module, 3)); fdputc (' ', g_stdout);
  fdputs ("locals: ", g_stdout); display_ (struct_ref_ (module, 4)); fdputc (' ', g_stdout);
  SCM table = struct_ref_ (module, 5);
  fdputs ("globals:\n  ", g_stdout);
  display_ (table);
  fdputc ('>', g_stdout);
}

SCM
module_variable (SCM module, SCM name)
{
  if (g_debug > 4)
    {
      eputs ("module_variable: "); display_error_ (name); eputs ("\n");
    }
  //SCM locals = struct_ref_ (module, 3);
  SCM locals = module;
  SCM x = assq (name, locals);
  if (x != cell_f)
      x = make_variable_ (x, 1);
  else
    {
      module = m0;
      SCM globals = struct_ref_ (module, 5);
      x = hashq_ref (globals, name, cell_f);
      if (x != cell_f)
        x = make_variable_ (x, 0);
    }
  return x;
}

SCM
module_ref (SCM module, SCM name)
{
  SCM x = module_variable (module, name);
  if (x == cell_f)
    return cell_undefined;
  x = VARIABLE (x);
  return CDR (x);
}

SCM
module_define_x (SCM module, SCM name, SCM value)
{
  if (g_debug > 4)
    {
      eputs ("module_define_x: "); display_error_ (name); eputs ("\n");
    }
  module = m0;
  SCM globals = struct_ref_ (module, 5);
  return hashq_set_x (globals, name, value);
}
