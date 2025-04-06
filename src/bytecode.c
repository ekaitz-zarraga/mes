/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2025 Ekaitz Zarraga <ekaitz@elenq.tech>
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
#include "mes/mes.h"

struct scm *
make_chunk_type ()                                          /*:((internal)) */
{
  struct scm *fields = cell_nil;
  fields = cons (cstring_to_symbol ("used"), fields);
  fields = cons (fields, cell_nil);
  fields = cons (cstring_to_symbol ("instrs"), fields);
  fields = cons (fields, cell_nil);
  fields = cons (cstring_to_symbol ("chunk"), fields);
  return make_struct (cell_symbol_record_type, fields, cell_unspecified);
}

struct scm *
make_chunk ()                                               /*:((internal)) */
{
  struct scm *chunk_type = make_chunk_type ();
  struct scm *values = cell_nil;
  values = cons (make_number (0), values);
  values = cons (make_vector_ (1, cell_unspecified), values);
  values = cons (cstring_to_symbol ("chunk"), values);
  return make_struct (chunk_type, values, cstring_to_symbol ("chunk"));
}

void
chunk_append (struct scm *bc, struct scm *ins)              /*:((internal)) */
{
  struct scm *new;
  struct scm *vec  = struct_ref_ (bc, 3);
  struct scm *used = struct_ref_ (bc, 4);
  long l = vec->length;
  if (used->value >= l)
    {
      new = make_vector_uninit_ (l*2);
      vector_copy_x_ (new, 0, vec, 0, l);
      vector_fill_x_ (new, cell_unspecified, l, l*2);
      struct_set_x_ (bc, 3, new);
      vec = new;
    }
  vector_set_x (vec, used, ins);
  struct_set_x_ (bc, 4, make_number (used->value + 1));
}

struct scm *
chunk_vector (struct scm *bc)                               /*:((internal)) */
{
  struct scm *vec  = struct_ref_ (bc, 3);
  struct scm *used = struct_ref_ (bc, 4);
  struct scm *res  = make_vector_uninit_ (used->value);
  vector_copy_x_ (res, 0, vec, 0, used->value);
  return res;
}

/*
 * We use a `chunk` internally for compilation
 * - It's a struct with a vector and a number for usage control.
 * - The vector is grown when needed.
 * - We don't return the `chunk`, but a vector with no empty positions.
 */
struct scm *
compile (struct scm *exp)
{
  /* NOTE: That's preventing us from being collected */
  R1 = exp;
  R2 = make_chunk ();

  long i = 200000;
  while (i = i - 1)
    {
      /* TODO: Compile me baby */
      chunk_append (R2, make_number (696969696));
      gc_check ();
    }

  return chunk_vector (R2);
}

struct scm *
run (struct scm *vec)
{
  return cell_unspecified;
}
