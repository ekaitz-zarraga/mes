/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Jeremiah Orians <jeremiah@pdp10.guru>
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
#include <stdio.h>
#include <string.h>

SCM
read_input_file_env_ (SCM e, SCM a)
{
  if (e == cell_nil)
    return e;
  return cons (e, read_input_file_env_ (read_env (a), a));
}

SCM
read_input_file_env (SCM a)
{
  return read_input_file_env_ (read_env (cell_nil), cell_nil);
}

int
reader_read_line_comment (int c)
{
  while (c != EOF)
    {
      if (c == '\n')
        return c;
      c = readchar ();
    }
  error (cell_symbol_system_error, make_string0 ("reader_read_line_comment"));
}

SCM reader_read_block_comment (int s, int c);
SCM reader_read_hash (int c, SCM a);
SCM reader_read_list (int c, SCM a);

int
reader_identifier_p (int c)
{
  return (c > ' ' && c <= '~' && c != '"' && c != ';' && c != '(' && c != ')' && c != EOF);
}

int
reader_end_of_word_p (int c)
{
  return (c == '"' || c == ';' || c == '(' || c == ')' || isspace (c) || c == EOF);
}

SCM
reader_read_identifier_or_number (int c)
{
  int i = 0;
  long n = 0;
  int negative_p = 0;
  if (c == '+')
    if (isdigit (peekchar ()) != 0)
      c = readchar ();
  if (c == '-')
    if (isdigit (peekchar ()) != 0)
      {
        negative_p = 1;
        c = readchar ();
      }
  while (isdigit (c) != 0)
    {
      g_buf[i] = c;
      i = i + 1;
      n = n * 10;
      n = n + c - '0';
      c = readchar ();
    }
  if (reader_end_of_word_p (c))
    {
      unreadchar (c);
      if (negative_p != 0)
        n = 0 - n;
      return make_number (n);
    }
  /* Fallthrough: Note that `4a', `+1b' are identifiers */
  while (reader_end_of_word_p (c) == 0)
    {
      g_buf[i] = c;
      i = i + 1;
      c = readchar ();
    }
  unreadchar (c);
  g_buf[i] = 0;
  return cstring_to_symbol (g_buf);
}

SCM
reader_read_sexp_ (int c, SCM a)
{
reset_reader:
  if (c == EOF)
    return cell_nil;
  if (c == ';')
    {
      c = reader_read_line_comment (c);
      goto reset_reader;
    }
  if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\f'))
    {
      c = readchar ();
      goto reset_reader;
    }
  if (c == '(')
    return reader_read_list (readchar (), a);
  if (c == ')')
    return cell_nil;
  if (c == '#')
    return reader_read_hash (readchar (), a);
  if (c == '`')
    return cons (cell_symbol_quasiquote, cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if (c == ',')
    {
      if (peekchar () == '@')
        {
          readchar ();
          return cons (cell_symbol_unquote_splicing, cons (reader_read_sexp_ (readchar (), a), cell_nil));
        }
      return cons (cell_symbol_unquote, cons (reader_read_sexp_ (readchar (), a), cell_nil));
    }
  if (c == '\'')
    return cons (cell_symbol_quote, cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if (c == '"')
    return reader_read_string ();
  if (c == '.')
    if (reader_identifier_p (peekchar ()) == 0)
      return cell_dot;
  return reader_read_identifier_or_number (c);
}

int
reader_eat_whitespace (int c)
{
  while (isspace (c) != 0)
    c = readchar ();
  if (c == ';')
    return reader_eat_whitespace (reader_read_line_comment (c));
  if (c == '#')
    {
      int p = peekchar ();
      if (p == '!' || p == '|')
        {
          c = readchar ();
          reader_read_block_comment (c, readchar ());
          return reader_eat_whitespace (readchar ());
        }
    }
  return c;
}

SCM
reader_read_list (int c, SCM a)
{
  c = reader_eat_whitespace (c);
  if (c == ')')
    return cell_nil;
  if (c == EOF)
    error (cell_symbol_not_a_pair, make_string0 ("EOF in list"));
  SCM s = reader_read_sexp_ (c, a);
  if (s == cell_dot)
    {
      s = reader_read_list (readchar (), a);
      return CAR (s);
    }
  return cons (s, reader_read_list (readchar (), a));
}

SCM
read_env (SCM a)
{
  return reader_read_sexp_ (readchar (), a);
}

SCM
reader_read_block_comment (int s, int c)
{
  if (c == s)
    if (peekchar () == '#')
      return readchar ();
  return reader_read_block_comment (s, readchar ());
}

SCM
reader_read_hash (int c, SCM a)
{
  if (c == '!')
    {
      reader_read_block_comment (c, readchar ());
      return reader_read_sexp_ (readchar (), a);
    }
  if (c == '|')
    {
      reader_read_block_comment (c, readchar ());
      return reader_read_sexp_ (readchar (), a);
    }
  if (c == 'f')
    return cell_f;
  if (c == 't')
    return cell_t;
  if (c == ',')
    {
      if (peekchar () == '@')
        {
          readchar ();
          return cons (cell_symbol_unsyntax_splicing, cons (reader_read_sexp_ (readchar (), a), cell_nil));
        }

      return cons (cell_symbol_unsyntax, cons (reader_read_sexp_ (readchar (), a), cell_nil));
    }
  if (c == '\'')
    return cons (cell_symbol_syntax, cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if (c == '`')
    return cons (cell_symbol_quasisyntax, cons (reader_read_sexp_ (readchar (), a), cell_nil));
  if (c == ':')
    {
      SCM x = reader_read_identifier_or_number (readchar ());
      SCM msg = make_string0 ("keyword perifx ':' not followed by a symbol: ");
      if (TYPE (x) == TNUMBER)
        error (cell_symbol_system_error, cons (msg, x));
      return symbol_to_keyword (x);
    }
  if (c == 'b')
    return reader_read_binary ();
  if (c == 'o')
    return reader_read_octal ();
  if (c == 'x')
    return reader_read_hex ();
  if (c == '\\')
    return reader_read_character ();
  if (c == '(')
    return list_to_vector (reader_read_list (readchar (), a));
  if (c == ';')
    {
      reader_read_sexp_ (readchar (), a);
      return reader_read_sexp_ (readchar (), a);
    }
  return reader_read_sexp_ (readchar (), a);
}

SCM
reader_read_sexp (SCM c, SCM s, SCM a)
{
  return reader_read_sexp_ (VALUE (c), a);
}

SCM
reader_read_character ()
{
  int c = readchar ();
  int p = peekchar ();
  int i = 0;
  if (c >= '0' && c <= '7' && p >= '0' && p <= '7')
    {
      c = c - '0';
      while (p >= '0' && p <= '7')
        {
          c = c << 3;
          c = c + readchar () - '0';
          p = peekchar ();
        }
    }
  else if (c == 'x' && ((p >= '0' && p <= '9') || (p >= 'a' && p <= 'f') || (p >= 'F' && p <= 'F')))
    {
      SCM n = reader_read_hex ();
      c = VALUE (n);
      eputs ("reading hex c=");
      eputs (itoa (c));
      eputs ("\n");
    }
  else if (((c >= 'a' && c <= 'z') || c == '*') && ((p >= 'a' && p <= 'z') || p == '*'))
    {
      char *buf = __reader_read_char_buf;
      buf[i] = c;
      i = i + 1;
      while ((p >= 'a' && p <= 'z') || p == '*')
        {
          buf[i] = readchar ();
          i = i + 1;
          p = peekchar ();
        }
      buf[i] = 0;
      if (strcmp (buf, "*eof*") == 0)
        c = EOF;
      else if (strcmp (buf, "nul") == 0)
        c = '\0';
      else if (strcmp (buf, "alarm") == 0)
        c = '\a';
      else if (strcmp (buf, "backspace") == 0)
        c = '\b';
      else if (strcmp (buf, "tab") == 0)
        c = '\t';
      else if (strcmp (buf, "linefeed") == 0)
        c = '\n';
      else if (strcmp (buf, "newline") == 0)
        c = '\n';
      else if (strcmp (buf, "vtab") == 0)
        c = '\v';
      else if (strcmp (buf, "page") == 0)
        c = '\f';
      else if (strcmp (buf, "return") == 0)
        /* Nyacc bug
           c = '\r'; */
        c = 13;
      else if (strcmp (buf, "esc") == 0)
        c = 27;
      else if (strcmp (buf, "space") == 0)
        c = ' ';
      /* Nyacc uses old abbrevs */
      else if (strcmp (buf, "bel") == 0)
        c = '\a';
      else if (strcmp (buf, "bs") == 0)
        c = '\b';
      else if (strcmp (buf, "ht") == 0)
        c = '\t';
      else if (strcmp (buf, "vt") == 0)
        c = '\v';
      else if (strcmp (buf, "cr") == 0)
        /* Nyacc bug
           c = '\r'; */
        c = 13;
      else
        {
          eputs ("char not supported: ");
          eputs (buf);
          eputs ("\n");
          error (cell_symbol_system_error, make_string0 ("char not supported"));
        }
    }
  return make_char (c);
}

SCM
reader_read_binary ()
{
  long n = 0;
  int c = peekchar ();
  int negative_p = 0;
  if (c == '-')
    {
      negative_p = 1;
      readchar ();
      c = peekchar ();
    }
  while (c == '0' || c == '1')
    {
      n = n << 1;
      n = n + c - '0';
      readchar ();
      c = peekchar ();
    }
  if (negative_p != 0)
    n = 0 - n;
  return make_number (n);
}

SCM
reader_read_octal ()
{
  long n = 0;
  int c = peekchar ();
  int negative_p = 0;
  if (c == '-')
    {
      negative_p = 1;
      readchar ();
      c = peekchar ();
    }
  while (c >= '0' && c <= '7')
    {
      n = n << 3;
      n = n + c - '0';
      readchar ();
      c = peekchar ();
    }
  if (negative_p != 0)
    n = 0 - n;
  return make_number (n);
}

SCM
reader_read_hex ()
{
  long n = 0;
  int c = peekchar ();
  int negative_p = 0;
  if (c == '-')
    {
      negative_p = 1;
      readchar ();
      c = peekchar ();
    }
  while ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
    {
      n = n << 4;
      if (c >= 'a')
        n = n + c - 'a' + 10;
      else if (c >= 'A')
        n = n + c - 'A' + 10;
      else
        n = n + c - '0';
      readchar ();
      c = peekchar ();
    }
  if (negative_p != 0)
    n = 0 - n;
  return make_number (n);
}

SCM
reader_read_string ()
{
  size_t i = 0;
  int c;
  do
    {
      if (i > MAX_STRING)
        assert_max_string (i, "reader_read_string", g_buf);
      c = readchar ();
      if (c == '"')
        break;
      if (c == '\\')
        {
          c = readchar ();
          if (c == '\\' || c == '"')
            0;
          else if (c == '0')
            c = '\0';
          else if (c == 'a')
            c = '\a';
          else if (c == 'b')
            c = '\b';
          else if (c == 't')
            c = '\t';
          else if (c == 'n')
            c = '\n';
          else if (c == 'v')
            c = '\v';
          else if (c == 'f')
            c = '\f';
          else if (c == 'r')
            /* Nyacc bug
               c = '\r'; */
            c = 13;
          else if (c == 'e')
            /* Nyacc bug
               c = '\e'; */
            c = 27;
          else if (c == 'x')
            {
              SCM n = reader_read_hex ();
              c = VALUE (n);
            }
        }
      g_buf[i] = c;
      i = i + 1;
    }
  while (1);
  g_buf[i] = 0;
  return make_string (g_buf, i);
}
