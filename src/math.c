/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
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

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

void
assert_number (char const *name, struct scm *x)
{
  if (x->type != TNUMBER)
    {
      eputs (name);
      error (cell_symbol_not_a_number, x);
    }
}

#define CAR(x) x->car
#define CDR(x) x->cdr
#define VALUE(x) x->value
#define TYPE(x) x->type
#define MAKE_NUMBER(x) make_number (x)

struct scm *
greater_p (struct scm *x)               /*:((name . ">") (arity . n)) */
{
  if (x == cell_nil)
    return cell_t;
  assert_number ("greater_p", x->car);
  long n = x->car->value;
  x = x->cdr;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      assert_number ("greater_p", x->car);
      i = car (x);
      v = i->value;
      if (v >= n)
        return cell_f;
      n = v;
      x = cdr (x);
    }
  return cell_t;
}

struct scm *
less_p (struct scm *x)                  /*:((name . "<") (arity . n)) */
{
  if (x == cell_nil)
    return cell_t;
  assert_number ("less_p", x->car);
  long n = x->car->value;
  x = x->cdr;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      assert_number ("less_p", x->car);
      i = car (x);
      v = i->value;
      if (v <= n)
        return cell_f;
      n = v;
      x = cdr (x);
    }
  return cell_t;
}

struct scm *
is_p (struct scm *x)                    /*:((name . "=") (arity . n)) */
{
  if (x == cell_nil)
    return cell_t;
  assert_number ("is_p", x->car);
  long n = x->car->value;
  x = cdr (x);
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      i = car (x);
      v = i->value;
      if (v != n)
        return cell_f;
      x = cdr (x);
    }
  return cell_t;
}

struct scm *
minus (struct scm *x)                   /*:((name . "-") (arity . n)) */
{
  assert_number ("minus", x->car);
  long n = x->car->value;
  x = cdr (x);
  if (x == cell_nil)
    n = -n;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      i = car (x);
      assert_number ("minus", i);
      v = i->value;
      n = n - v;
      x = cdr (x);
    }
  return make_number (n);
}

struct scm *
plus (struct scm *x)                    /*:((name . "+") (arity . n)) */
{
  long n = 0;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      i = car (x);
      assert_number ("plus", i);
      v = i->value;
      n = n + v;
      x = cdr (x);
    }
  return make_number (n);
}

struct scm *
divide_023 (struct scm *x)                  /*:((name . "/") (arity . n)) */
{
  long n = 1;
  if (x != cell_nil)
    {
      assert_number ("divide", CAR (x));
      n = VALUE (car (x));
      x = cdr (x);
    }
  while (x != cell_nil)
    {
      assert_number ("divide", CAR (x));
      long y = VALUE (CAR (x));
      if (y == 0)
        error (cstring_to_symbol ("divide-by-zero"), x);
      if (!n)
        break;
      n /= y;
      x = cdr (x);
    }
  return MAKE_NUMBER (n);
}

struct scm *
divide_024 (struct scm *x)                  /*:((name . "/") (arity . n)) */
{
  long n = 1;
  struct scm *i;
  long v;
  if (x != cell_nil)
    {
      i = car (x);
      assert_number ("divide", i);
      v = i->value;
      n = v;
      x = cdr (x);
    }
  int sign_p = 0;
  size_t u = n;
  if (n < 0)
    {
      sign_p = 1;
      u = -n;
    }
  size_t w;
  while (x != cell_nil)
    {
      i = car (x);
      assert_number ("divide", i);
      v = i->value;
      sign_p = sign_p && v > 0 || !sign_p && v < 0;
      w = v;
      if (v == 0)
        error (cstring_to_symbol ("divide-by-zero"), x);
      if (u == 0)
        break;
      if (w != 1)
        u = u / w;
      x = cdr (x);
    }
  n = u;
  if (sign_p)
    n = -n;
  return make_number (n);
}

struct scm *
divide (struct scm *a)
{
  struct scm* r23 = divide_023 (a);
  struct scm* r24 = divide_024 (a);
  if (r23->value != r24->value)
    {
      eputs ("divide!\n");
      eputs ("a="); eputs (itoa (a->car->value)); eputs (",");
      eputs ("b="); eputs (itoa (a->cdr->car->value)); eputs ("\n");
      eputs ("23="); eputs (itoa (r23->value)); eputs ("\n");
      eputs ("24="); eputs (itoa (r24->value)); eputs ("\n");
    }
  return r24;
}

struct scm *
modulo_023 (struct scm *a, struct scm *b)
{
  assert_number ("modulo", a);
  assert_number ("modulo", b);
  long x = VALUE (a);
  long y = VALUE (b);
  if (y == 0)
    error (cstring_to_symbol ("divide-by-zero"), a);
  while (x < 0)
    x += y;
  x = x ? x % y : 0;
  return MAKE_NUMBER (x);
}

struct scm *
modulo_024 (struct scm *a, struct scm *b)
{
  assert_number ("modulo", a);
  assert_number ("modulo", b);
  long n = a->value;
  long v = b->value;
  if (v == 0)
    error (cstring_to_symbol ("divide-by-zero"), a);
  int sign_p = 0;
  size_t w = v;
  if (v < 0)
    {
      sign_p = 1;
      w = -v;
    }
  while (n < 0)
    n = n + w;
  // size_t u = n;
  // u = n;
  // if (u != 0)
  //   u = u % w;
  // n = u;
  if (n != 0)
    n = n % w;
  if (sign_p)
    n = -n;
  return make_number (n);
}

struct scm *
modulo (struct scm *a, struct scm *b)
{
  struct scm* r23 = modulo_023 (a, b);
  struct scm* r24 = modulo_024 (a, b);
  if (r23->value != r24->value)
    {
      eputs ("modulo!\n");
      eputs ("a="); eputs (itoa (a->value)); eputs (",");
      eputs ("b="); eputs (itoa (b->value)); eputs ("\n");
      eputs ("23="); eputs (itoa (r23->value)); eputs ("\n");
      eputs ("24="); eputs (itoa (r24->value)); eputs ("\n");
    }
  return r24;
}

struct scm *
multiply (struct scm *x)                /*:((name . "*") (arity . n)) */
{
  long n = 1;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      i = car (x);
      assert_number ("multiply", i);
      v = i->value;
      n = n * v;
      x = cdr (x);
    }
  return make_number (n);
}

struct scm *
logand (struct scm *x)                  /*:((arity . n)) */
{
  long n = -1;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      i = car (x);
      assert_number ("multiply", i);
      v = i->value;
      n = n & v;
      x = cdr (x);
    }
  return make_number (n);
}

struct scm *
logior (struct scm *x)                  /*:((arity . n)) */
{
  long n = 0;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      i = car (x);
      assert_number ("logior", i);
      v = i->value;
      n = n | v;
      x = cdr (x);
    }
  return make_number (n);
}

struct scm *
lognot (struct scm *x)
{
  assert_number ("lognot", x);
  long n = ~x->value;
  return make_number (n);
}

struct scm *
logxor (struct scm *x)                  /*:((arity . n)) */
{
  long n = 0;
  struct scm *i;
  long v;
  while (x != cell_nil)
    {
      i = car (x);
      assert_number ("logxor", i);
      v = i->value;
      n = n ^ v;
      x = cdr (x);
    }
  return make_number (n);
}

struct scm *
ash (struct scm *n, struct scm *count)
{
  assert_number ("ash", n);
  assert_number ("ash", count);
  long cn = n->value;
  long ccount = count->value;
  long result;
  if (ccount < 0)
    result = cn >> -ccount;
  else
    result = cn << ccount;
  return make_number (result);
}
