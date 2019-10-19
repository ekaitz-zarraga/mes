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

#include <limits.h>
#include <string.h>

int
string_len (char *a)
{
  int i = 0;
  while (0 != a[i])
    i = i + 1;
  return i;
}

char const *
list_to_cstring (struct scm *list, int *size)
{
  int i = 0;

  while (list != cell_nil)
    {
      assert_max_string (i, "list_to_string", g_buf);

      g_buf[i] = list->car->value;
      i = i + 1;
      list = list->cdr;
    }

  g_buf[i] = 0;
  *size = i;
  return g_buf;
}

struct scm *
make_string_ (char *s)          /* internal only */
{
  SCM l = string_len (s);
  assert_max_string (l, "make_string_", s);

  struct scm *y = make_tstring1 (l);
  y->cdr = make_bytes (s, l);
  return y;
}

struct scm *
make_string (char const *s, int length)
{
  assert_max_string (length, "make_string", (char *) s);
  struct scm *x = make_tstring1 (length);
  struct scm *y = x;
  struct scm *v = make_bytes (s, length);
  y->cdr = v;
  return y;
}

struct scm *
string_equal_p (struct scm *a, struct scm *b)
{
  struct scm *a2 = a;
  struct scm *b2 = b;
  assert (a2->type == TSTRING || a2->type == TKEYWORD);
  assert (b2->type == TSTRING || b2->type == TKEYWORD);
  struct scm *tee = cell_t;
  struct scm *nil = cell_f;

  /* If they are the same thing */
  if (a == b)
    return tee;

  /* If they point to the same string */
  if (a2->cdr == b2->cdr)
    return tee;

  /*If they are both empty strings */
  if ((NULL == a2->car) && (NULL == b2->car))
    return tee;

  /* If they are different lengths they can't be the same string */
  if (a2->length != b2->length)
    return nil;

  /* Need to fix */
  char *s1 = a2->cdr->string;
  char *s2 = b2->cdr->string;

  while (s1[0] == s2[0])
    {
      if (0 == s1[0])
        return tee;
      s1 = s1 + 1;
      s2 = s2 + 1;
    }

  return nil;
}

struct scm *
symbol_to_string (struct scm *symbol)
{
  struct scm *a = symbol;
  return make_tstring2 (a->car, a->cdr);
}

struct scm *
symbol_to_keyword (struct scm *symbol)
{
  struct scm *a = symbol;
  return make_keyword (a->car, a->cdr);
}

struct scm *
make_symbol (struct scm *string)
{
  struct scm *s = string;
  struct scm *x = make_tsymbol (s->car, s->cdr);
  hash_set_x (g_symbols, string, x);
  return x;
}

struct scm *
string_to_symbol (struct scm *string)
{
  struct scm *x = hash_ref (g_symbols, string, cell_f);

  if (x == cell_f)
    {
      x = make_symbol (string);
    }

  return x;
}

struct scm *
cstring_to_symbol (char *s)
{
  struct scm *string = make_string_ (s);
  return string_to_symbol (string);
}

/* EXTERNAL */

struct scm *
string_equal_p_ (struct scm *a, struct scm *b)
{
  return string_equal_p (a, b);
}

struct scm *
symbol_to_string_ (struct scm *symbol)
{
  return symbol_to_string (symbol);
}

struct scm *
symbol_to_keyword_ (struct scm *symbol)
{
  return symbol_to_keyword (symbol);
}

struct scm *
keyword_to_string (struct scm *keyword)
{
  struct scm *a = keyword;
  return make_tstring2 (a->car, a->cdr);
}

struct scm *
make_symbol_ (struct scm *string)
{
  return make_symbol (string);
}

struct scm *
string_to_list (struct scm *string)
{
  struct scm *x = string;
  char *s = x->cdr->string;
  SCM i = string_len (s);
  struct scm *p = cell_nil;

  while (0 != i)
    {
      i = i - 1;
      int c = (0xFF & s[i]);
      p = cons (make_char (c), p);
    }

  return p;
}

struct scm *
list_to_string (struct scm *list)
{
  int size;
  char const *s = list_to_cstring (list, &size);
  return make_string (s, size);
}

void
block_copy (void *source, void *destination, int num)
{
  char *s;
  char *d = destination;
  for (s = source; 0 < num; num = num - 1)
    {
      d[0] = s[0];
      d = d + 1;
      s = s + 1;
    }
}

struct scm *
string_append (struct scm *x)   /*((arity . n)) */
{
  char *p = g_buf;
  g_buf[0] = 0;
  int size = 0;
  struct scm *y1 = x;

  while (y1 != cell_nil)
    {
      struct scm *y2 = y1->car;
      assert (y2->type == TSTRING);
      memcpy (p, y2->cdr->string, y2->rac + 1);
      p += y2->length;
      size += y2->length;

      assert_max_string (size, "string_append", g_buf);

      y1 = y1->cdr;
    }

  return make_string (g_buf, size);
}

struct scm *
string_length (struct scm *string)
{
  struct scm *x = string;
  assert (x->type == TSTRING);
  return make_number (x->length);
}

struct scm *
string_ref (struct scm *str, struct scm *k)
{
  struct scm *x = str;
  struct scm *y = k;
  assert (x->type == TSTRING);
  assert (y->type == TNUMBER);
  size_t size = x->length;
  size_t i = y->value;

  if (i > size)
    {
      error (cell_symbol_system_error,
             cons (make_string ("value out of range", string_len ("value out of range")), k));
    }

  char *p = x->cdr->string;
  return make_char (p[i]);
}
