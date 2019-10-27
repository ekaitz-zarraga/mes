/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <errno.h>
#include <string.h>
#include <stdlib.h>

// long ARENA_SIZE;
// long MAX_ARENA_SIZE;
// long STACK_SIZE;
// long JAM_SIZE;
// long GC_SAFETY;
// long MAX_STRING;
// char *g_arena;
// long g_free;

void init_symbols_ ();

#if __M2_PLANET__
#define M2_CELL_SIZE 12
// CONSTANT M2_CELL_SIZE 12
#else
#define M2_CELL_SIZE 1
// CONSTANT M2_CELL_SIZE 12
#endif

#if POINTER_CELLS
long g_stack;
#else
SCM g_stack;
#endif

#if POINTER_CELLS
SCM g_symbol;
#else
long g_symbol;
#endif

SCM *g_stack_array;

struct scm *g_cells;
struct scm *g_news;

char *
cell_bytes (SCM x)
{
#if POINTER_CELLS
  char *p = x;
  return p + (2 * sizeof (long));
#elif __M2_PLANET__
  CELL (x) + 8;
#else
  return &CDR (x);
#endif
}

char *
news_bytes (SCM x)
{
  return &NCDR (x);
}

SCM
gc_init ()                      /*:((internal)) */
{
#if SYSTEM_LIBC
  ARENA_SIZE = 100000000;       // 2.3GiB
#else
  ARENA_SIZE = 300000;          // 32b: 3MiB, 64b: 6 MiB
#endif
  MAX_ARENA_SIZE = 100000000;
  STACK_SIZE = 20000;

#if POINTER_CELLS
  JAM_SIZE = 1000;
#else
  JAM_SIZE = 20000;
#endif
  GC_SAFETY = 2000;
  MAX_STRING = 524288;

  char *p;
  if (p = getenv ("MES_MAX_ARENA"))
    MAX_ARENA_SIZE = atoi (p);
  if (p = getenv ("MES_ARENA"))
    ARENA_SIZE = atoi (p);
  JAM_SIZE = ARENA_SIZE / 10;
  if (p = getenv ("MES_JAM"))
    JAM_SIZE = atoi (p);
  GC_SAFETY = ARENA_SIZE / 100;
  if (p = getenv ("MES_SAFETY"))
    GC_SAFETY = atoi (p);
  if (p = getenv ("MES_STACK"))
    STACK_SIZE = atoi (p);
  if (p = getenv ("MES_MAX_STRING"))
    MAX_STRING = atoi (p);

  long arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
#if POINTER_CELLS
  void *a = malloc (arena_bytes + (STACK_SIZE * sizeof (SCM) * 2));
#else
  void *a = malloc (arena_bytes + (STACK_SIZE * sizeof (SCM)));
#endif
  g_cells = a;
  g_stack_array = a + arena_bytes;

#if POINTER_CELLS
  /* The vector that holds the arenea. */
  cell_arena = g_cells;
#else
  /* The vector that holds the arenea. */
  cell_arena = 0;
#endif
  TYPE (cell_arena) = TVECTOR;
  LENGTH (cell_arena) = 1000;
  VECTOR (cell_arena) = 0;
  g_cells = g_cells + M2_CELL_SIZE;
  TYPE (cell_arena) = TCHAR;
  VALUE (cell_arena) = 'c';

#if POINTER_CELLS
  g_free = g_cells + M2_CELL_SIZE;
#else
  g_free = 1;
#endif
  g_symbols = 0;
  g_symbol_max = 0;
  g_macros = 0;
  g_ports = 0;
  g_symbol_max = 0;

  // FIXME: remove MES_MAX_STRING, grow dynamically
  g_buf = malloc (MAX_STRING);

  return 0;
}

long
gc_free ()
{
#if POINTER_CELLS
  return g_free - g_cells;
#else
  return g_free;
#endif
}

void
gc_stats_ (char const* where)
{
#if POINTER_CELLS
  long i = g_free - g_cells;
#else
  long i = g_free;
#endif
  eputs (where);
  eputs (": [");
  eputs (itoa (i));
  eputs ("]\n");
}

SCM
alloc (long n)
{
  SCM x = g_free;
  g_free = g_free + (n * M2_CELL_SIZE);
#if POINTER_CELLS
  long i = g_free - g_cells;
#else
  long i = g_free;
#endif
  if (i > ARENA_SIZE)
    assert_msg (0, "alloc: out of memory");
  return x;
}

SCM
make_cell (long type, SCM car, SCM cdr)
{
  SCM x = g_free;
  g_free = g_free + M2_CELL_SIZE;
#if POINTER_CELLS
  long i = g_free - g_cells;
#else
  long i = g_free;
#endif
  if (i > ARENA_SIZE)
    assert_msg (0, "alloc: out of memory");
  TYPE (x) = type;
  CAR (x) = car;
  CDR (x) = cdr;
  return x;
}

void
copy_cell (SCM to, SCM from)
{
  TYPE (to) = TYPE (from);
  CAR (to) = CAR (from);
  CDR (to) = CDR (from);
}

void
copy_news (SCM to, SCM from)
{
  NTYPE (to) = TYPE (from);
  NCAR (to) = CAR (from);
  NCDR (to) = CDR (from);
}

void
copy_stack (long index, SCM from)
{
  g_stack_array[index] = from;
}

SCM
cell_ref (SCM cell, long index)
{
  return cell + (index * M2_CELL_SIZE);
}

SCM
cons (SCM x, SCM y)
{
  return make_cell (TPAIR, x, y);
}

size_t
bytes_cells (size_t length)
{
  return (1 + sizeof (long) + sizeof (long) + length + sizeof (SCM)) / sizeof (SCM);
}

SCM
make_bytes (char const *s, size_t length)
{
  size_t size = bytes_cells (length);
  SCM x = alloc (size);
  TYPE (x) = TBYTES;
  LENGTH (x) = length;
  char *p = cell_bytes (x);
  if (length == 0)
    p[0] = 0;
  else
    memcpy (p, s, length + 1);

  return x;
}

SCM
make_char (int n)
{
  return make_cell (TCHAR, 0, n);
}

SCM
make_continuation (long n)
{
  return make_cell (TCONTINUATION, n, g_stack);
}

SCM
make_macro (SCM name, SCM x)    /*:((internal)) */
{
  return make_cell (TMACRO, x, STRING (name));
}

SCM
make_number (long n)
{
  return make_cell (TNUMBER, 0, n);
}

SCM
make_ref (SCM x)                /*:((internal)) */
{
  return make_cell (TREF, x, 0);
}

SCM
make_string (char const *s, size_t length)
{
  if (length > MAX_STRING)
    assert_max_string (length, "make_string", s);
  SCM x = make_cell (TSTRING, length, 0);
  SCM v = make_bytes (s, length);
  CDR (x) = v;
  return x;
}

SCM
make_string0 (char const *s)
{
  return make_string (s, strlen (s));
}

SCM
make_string_port (SCM x)        /*:((internal)) */
{
  return make_cell (TPORT, -length__ (g_ports) - 2, x);
}

SCM
gc_init_news ()                 /*:((internal)) */
{
#if POINTER_CELLS
  g_news = g_free;
#else
  g_news = g_cells + g_free;
  NTYPE (cell_arena) = TVECTOR;
  NLENGTH (cell_arena) = LENGTH (cell_arena - 1);
  NVECTOR (cell_arena) = 0;
  g_news = g_news + 1;
  NTYPE (cell_arena) = TCHAR;
  NVALUE (cell_arena) = 'n';
#endif
  return 0;
}

SCM
gc_up_arena ()                  /*:((internal)) */
{
  long old_arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  if (ARENA_SIZE >> 1 < MAX_ARENA_SIZE >> 2)
    {
      ARENA_SIZE = ARENA_SIZE << 1;
      JAM_SIZE = JAM_SIZE << 1;
      GC_SAFETY = GC_SAFETY << 1;
    }
  else
    ARENA_SIZE = MAX_ARENA_SIZE - JAM_SIZE;
  long arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  void *p = realloc (g_cells - M2_CELL_SIZE, (arena_bytes + STACK_SIZE) * sizeof (SCM));
  if (p == 0)
    {
      eputs ("realloc failed, g_free=");
      eputs (itoa (g_free));
      eputs (":");
#if POINTER_CELLS
      long i = g_free - g_cells;
#else
      long i = g_free;
#endif
      eputs (itoa (ARENA_SIZE - i));
      eputs ("\n");
      assert_msg (0, "0");
      exit (1);
    }
  g_cells = p;
  memcpy (p + arena_bytes, p + old_arena_bytes, STACK_SIZE * sizeof (SCM));
  g_cells = g_cells + M2_CELL_SIZE;

  return 0;
}

void
gc_flip ()
{
#if POINTER_CELLS
  //with pointers, nevva gonna wok
  //memcpy (g_cells - 1, g_news - 1, (g_free - g_cells + 2) * sizeof (struct scm));
  g_cells = g_news;
#endif
  if (g_debug > 2)
    gc_stats_ (";;; => jam");
#if POINTER_CELLS
  // nothing
#else
  if (g_free > JAM_SIZE)
    JAM_SIZE = g_free + g_free / 2;
  memcpy (g_cells - 1, g_news - 1, (g_free + 2) * sizeof (struct scm));
#endif
}

SCM
gc_copy (SCM old)               /*:((internal)) */
{
  if (TYPE (old) == TBROKEN_HEART)
    return CAR (old);
  SCM new = g_free;
  g_free = g_free + M2_CELL_SIZE;
  copy_news (new, old);
  if (NTYPE (new) == TSTRUCT || NTYPE (new) == TVECTOR)
    {
      NVECTOR (new) = g_free;
      long i;
      for (i = 0; i < LENGTH (old); i = i + 1)
        {
          copy_news (g_free, cell_ref (VECTOR (old), i));
          g_free = g_free + M2_CELL_SIZE;
        }
    }
  else if (NTYPE (new) == TBYTES)
    {
      char const *src = cell_bytes (old);
      char *dest = news_bytes (new);
#if POINTER_CELLS
      size_t length = LENGTH (old);
#else
      size_t length = NLENGTH (new);
#endif
      memcpy (dest, src, length + 1);
      g_free = g_free + ((bytes_cells (length) - 1) * M2_CELL_SIZE);

      if (g_debug > 4)
        {
          eputs ("gc copy bytes: ");
          eputs (src);
          eputs ("\n");
          eputs ("    length: ");
          eputs (itoa (LENGTH (old)));
          eputs ("\n");
          eputs ("    nlength: ");
          eputs (itoa (NLENGTH (new)));
          eputs ("\n");
          eputs ("        ==> ");
          eputs (dest);
          eputs ("\n");
        }
    }
  TYPE (old) = TBROKEN_HEART;
  CAR (old) = new;
  return new;
}

SCM
gc_relocate_car (SCM new, SCM car)      /*:((internal)) */
{
  NCAR (new) = car;
  return cell_unspecified;
}

SCM
gc_relocate_cdr (SCM new, SCM cdr)      /*:((internal)) */
{
  NCDR (new) = cdr;
  return cell_unspecified;
}

void
gc_loop (SCM scan)              /*:((internal)) */
{
  SCM car;
  SCM cdr;
  while (scan < g_free)
    {
      if (NTYPE (scan) == TBROKEN_HEART)
        error (cell_symbol_system_error, cstring_to_symbol ("gc"));
      if (NTYPE (scan) == TMACRO || NTYPE (scan) == TPAIR || NTYPE (scan) == TREF /* || scan == 1 //cell_nil */
          || NTYPE (scan) == TVARIABLE)
        {
          car = gc_copy (NCAR (scan));
          gc_relocate_car (scan, car);
        }
      if ((NTYPE (scan) == TCLOSURE || NTYPE (scan) == TCONTINUATION || NTYPE (scan) == TKEYWORD || NTYPE (scan) == TMACRO || NTYPE (scan) == TPAIR || NTYPE (scan) == TPORT || NTYPE (scan) == TSPECIAL || NTYPE (scan) == TSTRING || NTYPE (scan) == TSYMBOL /* || scan == 1 //cell_nil */
           || NTYPE (scan) == TVALUES)
          && NCDR (scan))   // allow for 0 terminated list of symbols
        {
          cdr = gc_copy (NCDR (scan));
          gc_relocate_cdr (scan, cdr);
        }
      if (NTYPE (scan) == TBYTES)
        scan = scan + ((bytes_cells (NLENGTH (scan)) - 1) * M2_CELL_SIZE);
      scan = scan + M2_CELL_SIZE;
    }
  gc_flip ();
}

SCM
gc_check ()
{
#if POINTER_CELLS
  if ((g_free - g_cells) + GC_SAFETY > ARENA_SIZE)
#else
  if (g_free + GC_SAFETY > ARENA_SIZE)
#endif
    gc ();
  return cell_unspecified;
}

SCM
gc_ ()                          /*:((internal)) */
{
  gc_init_news ();
  if (g_debug == 2)
    eputs (".");
  if (g_debug > 2)
    {
      gc_stats_ (";;; gc");
      eputs (";;; free: [");
#if POINTER_CELLS
      eputs (itoa (ARENA_SIZE - (g_free - g_cells)));
#else
      eputs (itoa (ARENA_SIZE - g_free));
#endif
      eputs ("]...");
    }
#if POINTER_CELLS
  g_free = g_news;
#else
  g_free = 1;
#endif

  if (ARENA_SIZE < MAX_ARENA_SIZE && g_news > 0)
    {
      if (g_debug == 2)
        eputs ("+");
      if (g_debug > 2)
        {
          eputs (" up[");
          eputs (itoa (g_cells));
          eputs (",");
          eputs (itoa (g_news));
          eputs (":");
          eputs (itoa (ARENA_SIZE));
          eputs (",");
          eputs (itoa (MAX_ARENA_SIZE));
          eputs ("]...");
        }
      gc_up_arena ();
    }

  SCM s;
  for (s = cell_nil; s < g_symbol_max; s = s + M2_CELL_SIZE)
    gc_copy (s);
  g_symbols = gc_copy (g_symbols);
  g_macros = gc_copy (g_macros);
  g_ports = gc_copy (g_ports);
  M0 = gc_copy (M0);
  long i;
  for (i = g_stack; i < STACK_SIZE; i = i + M2_CELL_SIZE)
    copy_stack (i, gc_copy (g_stack_array[i]));
#if POINTER_CELLS
  long save_gfree = g_free;
  long save_gsymbols =g_symbols;
  g_symbols = 0;
  ///g_free = g_news + 1;
  cell_nil = g_news; // hmm?
  init_symbols_ ();
  g_symbol_max = g_symbol;
  g_free = save_gfree;
  g_symbols = save_gsymbols;
#endif
  gc_loop (cell_nil);
}

SCM
gc ()
{
  if (g_debug > 5)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
      eputs ("\n");
      eputs ("R0: ");
      write_error_ (R0);
      eputs ("\n");
    }
  gc_push_frame ();
  gc_ ();
  gc_pop_frame ();
  if (g_debug > 5)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
      eputs ("\n");
      eputs ("R0: ");
      write_error_ (R0);
      eputs ("\n");
    }
}

SCM
gc_push_frame ()                /*:((internal)) */
{
  if (g_stack < 5)
    assert_msg (0, "STACK FULL");
  g_stack_array[g_stack - 1] = cell_f;
  g_stack_array[g_stack - 2] = R0;
  g_stack_array[g_stack - 3] = R1;
  g_stack_array[g_stack - 4] = R2;
  g_stack_array[g_stack - 5] = R3;
  g_stack = g_stack - 5;
  return g_stack;
}

SCM
gc_peek_frame ()                /*:((internal)) */
{
  R3 = g_stack_array[g_stack];
  R2 = g_stack_array[g_stack + 1];
  R1 = g_stack_array[g_stack + 2];
  R0 = g_stack_array[g_stack + 3];
  return g_stack_array[g_stack + FRAME_PROCEDURE];
}

SCM
gc_pop_frame ()                 /*:((internal)) */
{
  SCM x = gc_peek_frame ();
  g_stack = g_stack + 5;
  return x;
}
