/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019 Jeremiah Orians <jeremiah@pdp10.guru>
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

#include <stdlib.h>

struct scm *
exit_ (struct scm *x)           /* ((name . "exit")) */
{
  struct scm *y = x;
  require (TNUMBER == y->type, "exit_ in src/lib.c didn't recieve a number\n");
  exit (y->value);
}

struct scm *
make_frame_type ()              /* ((internal)) */
{
  return make_struct (cell_symbol_record_type,
                      cons (cell_symbol_frame, cons (cons (cell_symbol_procedure, cell_nil), cell_nil)),
                      cell_unspecified);
}


struct scm *
make_stack_type ()              /* ((internal)) */
{
  return make_struct (cell_symbol_record_type,
                      cons (cell_symbol_stack,
                            cons (cons (cstring_to_symbol ("frames"), cell_nil), cell_nil)),
                      cell_unspecified);
}

struct scm *
stack_length (struct scm *stack)
{
  return vector_length (struct_ref_ (stack, 3));
}

struct scm *
stack_ref (struct scm *stack, SCM index)
{
  struct scm *y = struct_ref_ (stack, 3);
  require (TVECTOR == y->type, "stack_ref in src/lib.c did not recieve a TVECTOR\n");
  require (index < y->length, "y->length in stack_ref in src/lib.c was less than or equal to index\n");
  struct scm *e = y->cdr + index;

  if (e->type == TREF)
    {
      return e->car;
    }

  if (e->type == TCHAR)
    {
      return make_char (e->value);
    }

  if (e->type == TNUMBER)
    {
      return make_number (e->value);
    }

  return e;
}

struct scm *
xassq (struct scm *x, struct scm *a)    /* for speed in core only */
{
  while (a != cell_nil && x != a->car->cdr)
    {
      a = a->cdr;
    }

  if (cell_nil == a)
    return cell_f;
  return a->car;
}

struct scm *
memq (struct scm *x, struct scm *a)
{
  int t = x->type;

  if (t == TCHAR || t == TNUMBER)
    {
      SCM v = x->value;

      while (a != cell_nil && v != a->car->value)
        {
          a = a->cdr;
        }
    }
  else if (t == TKEYWORD)
    {
      while (a != cell_nil && (a->car->type != TKEYWORD || string_equal_p (x, a->car) == cell_f))
        {
          a = a->cdr;
        }
    }
  else
    {
      while (a != cell_nil && x != a->car)
        {
          a = a->cdr;
        }
    }

  if (cell_nil == a)
    return cell_f;
  return a;
}

struct scm *
equal2_p (struct scm *a, struct scm *b)
{
  if (a == b)
    {
      return cell_t;
    }

  if (a->type == TPAIR && b->type == TPAIR)
    {
      if ((cell_t == equal2_p (a->car, b->car)) && (cell_t == equal2_p (a->cdr, b->cdr)))
        return cell_t;
      return cell_f;
    }

  if (a->type == TSTRING && b->type == TSTRING)
    {
      return string_equal_p (a, b);
    }

  if (a->type == TVECTOR && b->type == TVECTOR)
    {
      return vector_equal_p (a, b);
    }

  return eq_p (a, b);
}

struct scm *
last_pair (struct scm *x)
{
  while (x != cell_nil && x->cdr != cell_nil)
    {
      x = x->cdr;
    }

  return x;
}

struct scm *
pair_p (struct scm *x)
{
  if (TPAIR == x->type)
    return cell_t;
  return cell_f;
}
