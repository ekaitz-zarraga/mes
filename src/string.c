/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020,2022 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2023 Timothy Sample <samplet@ngyro.com>
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

#include <limits.h>
#include <string.h>

struct scm *
string_equal_p (struct scm *a, struct scm *b)   /*:((name . "string=?")) */
{
  if (!((a->type == TSTRING && b->type == TSTRING) || (a->type == TKEYWORD || b->type == TKEYWORD)))
    {
      eputs ("type a: ");
      eputs (itoa (a->type));
      eputs ("\n");
      eputs ("type b: ");
      eputs (itoa (b->type));
      eputs ("\n");
      eputs ("a= ");
      write_error_ (a);
      eputs ("\n");
      eputs ("b= ");
      write_error_ (b);
      eputs ("\n");
      assert_msg ((a->type == TSTRING && b->type == TSTRING) || (a->type == TKEYWORD || b->type == TKEYWORD), "(a->type == TSTRING && b->type == TSTRING) || (a->type == TKEYWORD || b->type == TKEYWORD)");
    }
  if (a == b)
    return cell_t;
  if (a->string == b->string)
    return cell_t;
  if (a->length == 0 && b->length == 0)
    return cell_t;
  if (a->length == b->length)
    if (memcmp (cell_bytes (a->string), cell_bytes (b->string), a->length) == 0)
      return cell_t;

  return cell_f;
}

struct scm *
symbol_to_string (struct scm *symbol)
{
  return make_cell (TSTRING, symbol->car, symbol->cdr);
}

struct scm *
symbol_to_keyword (struct scm *symbol)
{
  return make_cell (TKEYWORD, symbol->car, symbol->cdr);
}

struct scm *
keyword_to_string (struct scm *keyword)
{
  return make_cell (TSTRING, keyword->car, keyword->cdr);
}

struct scm *
string_to_symbol (struct scm *string)
{
  struct scm *x = hash_ref_ (g_symbols, string, cell_f);
  if (x == cell_f)
    x = make_symbol (string);
  return x;
}

struct scm *
make_symbol (struct scm *string)
{
  struct scm *x = make_pointer_cell (TSYMBOL, string->length, string->string);
  hash_set_x (g_symbols, string, x);
  return x;
}

struct scm *
bytes_to_list (char const *s, size_t i)
{
  struct scm *p = cell_nil;
  int c;
  while (i != 0)
    {
      i = i - 1;
      c = (0x100 + s[i]) % 0x100;
      p = cons (make_char (c), p);
    }
  return p;
}

struct scm *
cstring_to_list (char const *s)
{
  return bytes_to_list (s, strlen (s));
}

struct scm *
cstring_to_symbol (char const *s)
{
  struct scm *string = make_string0 (s);
  return string_to_symbol (string);
}

struct scm *
string_to_list (struct scm *string)
{
  return bytes_to_list (cell_bytes (string->string), string->length);
}

struct scm *
list_to_string (struct scm *list)
{
  struct scm *res;
  struct scm *x;
  res = make_string_init_ (length__ (list), '\0');
  size_t i = 0;
  while (list != cell_nil)
    {
      x = car (list);
      string_set_x_ (res, i, x->value);
      i = i + 1;
      list = cdr (list);
    }
  return res;
}

struct scm *
read_string (struct scm *port)          /*:((arity . n)) */
{
  int fd = __stdin;
  struct scm *res = make_string_init_ (512, '\0');
  if (port->type == TPAIR)
    {
      struct scm *p = car (port);
      if (p->type == TNUMBER)
        __stdin = p->value;
    }
  int c = readchar ();
  size_t i = 0;
  while (c != -1)
    {
      if (i >= res->length)
        res = string_resize (res, res->length * 2);

      string_set_x_ (res, i, c);
      i = i + 1;
      c = readchar ();
    }
  __stdin = fd;
  return string_resize (res, i);
}

struct scm *
string_resize (struct scm *x, size_t size)
{
  // TODO: Optimization: make the string uninitialized?
  struct scm *y = make_string_init_ (size, '\0');
  size_t l;
  if (x->length > size)
    l = size;
  else
    l = x->length;
  string_copy_x_ (y, 0, x, 0, l);
  return y;
}

struct scm *
string_append (struct scm *args)           /*:((arity . n)) */
{
  size_t size = 0;
  struct scm *string;
  struct scm *res;
  struct scm *x = args;
  while (x != cell_nil)
    {
      assert_msg (string->type == TSTRING, "string->type == TSTRING");
      string = x->car;
      size = size + string->length;
      x = x->cdr;
    }
  res = make_string_init_ (size, '\0');
  size = 0;
  x = args;
  while (x != cell_nil)
    {
      string = x->car;
      string_copy_x_ (res, size, string, 0, string->length);
      size = size + string->length;
      x = x->cdr;
    }
  return res;
}

struct scm *
string_length (struct scm *string)
{
  assert_msg (string->type == TSTRING, "string->type == TSTRING");
  return make_number (string->length);
}

char
string_ref_ (struct scm *str, long i)
{
  size_t size = str->length;
  char const *p = cell_bytes (str->string);
  if (i > size)
    error (cell_symbol_system_error, cons (make_string0 ("value out of range"), make_number (i)));
  return p[i];
}

struct scm *
string_ref (struct scm *str, struct scm *k)
{
  assert_msg (str->type == TSTRING, "str->type == TSTRING");
  assert_msg (k->type == TNUMBER, "k->type == TNUMBER");
  return make_char (string_ref_ (str, k->value));
}

struct scm *
string_set_x_ (struct scm *str, long i, char c)
{
  char *p = cell_bytes (str->string);
  size_t size = str->length;
  if (i > size)
    error (cell_symbol_system_error, cons (make_string0 ("value out of range"), make_number (i)));
  p[i] = c;
  return cell_unspecified;
}

struct scm *
string_set_x (struct scm *str, struct scm *k, struct scm *c)
{
  assert_msg (str->type == TSTRING, "str->type == TSTRING");
  assert_msg (k->type == TNUMBER, "k->type == TNUMBER");
  assert_msg (c->type == TCHAR, "c->type == TCHAR");
  return string_set_x_ (str, k->value, c->value);
}

struct scm *
string_copy_x_ (struct scm *str, long start, struct scm *source, long begin,
              long end)
{
  assert_msg (str->length - start >= end - begin,
              "str->length - start >= end - begin");
  long i;
  for (i = begin; i < end; i = i + 1)
    /* TODO: Optimization: Maybe copy the bytes with memcpy? Is that faster? */
    string_set_x_ (str, i - begin + start, string_ref_ (source, i));
  return cell_undefined;
}

struct scm *
string_copy_x (struct scm *x)               /*:((arity . n)) */
{
  /* Arguments */
  struct scm *str;
  struct scm *start;
  struct scm *source;
  struct scm *begin; /* optional */
  struct scm *end;   /* optional */

  str = x->car;
  assert_msg (str->type == TSTRING, "str->type == TSTRING");
  x = x->cdr;

  assert_msg (x->type == TPAIR, "x->type == TPAIR");
  start = x->car;
  assert_number("string_copy_x: start", start);
  x = x->cdr;

  assert_msg (x->type == TPAIR, "x->type == TPAIR");
  source = x->car;
  assert_msg (source->type == TSTRING, "source->type == TSTRING");
  x = x->cdr;

  if (x->type != TPAIR)
    return string_copy_x_ (str, start->value, source, 0, source->length);
  begin = x->car;
  assert_number("string_copy_x: begin", begin);
  x = x->cdr;

  if (x->type != TPAIR)
    return string_copy_x_ (str, start->value, source, begin->value, source->length);
  end = x->car;
  assert_number("string_copy_x: end", end);

  return string_copy_x_ (str, start->value, source, begin->value, end->value);
}

struct scm *
make_string_init_ (long length, char c)
{
  struct scm *x = make_pointer_cell (TSTRING, length, 0);
  struct scm *v = make_bytes (length + 1);
  char *p = cell_bytes (v);
  memset (p, c, length + 1);
  x->cdr = v;
  return x;
}

struct scm *
make_string_init (struct scm *x)
{
  struct scm *k = x->car;
  assert_number ("make-string", k);
  long n = k->value;
  char c = '\0';
  if (x->cdr != cell_nil)
    {
      x = x->cdr->car;
      assert_msg (x->type == TCHAR, "x->type == TCHAR");
      c = x->value;
    }

  return make_string_init_ (n, c);
}
