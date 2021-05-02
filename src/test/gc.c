/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <string.h>

#if POINTER_CELLS
#define M2_CELL_SIZE 1
// CONSTANT M2_CELL_SIZE 12
#else
#define M2_CELL_SIZE 1
// CONSTANT M2_CELL_SIZE 1
#endif

int g_debug;

void
test_empty ()
{
  g_free = g_symbol_max;

  SCM v = cell_arena;
  LENGTH (v) = gc_free () - 1;
  eputs ("arena["); eputs (ntoab (g_cells, 16, 0)); eputs ("]: "); write_ (v); eputs ("\n");
  gc_stats_ ("0");
  gc_ ();
  v = cell_arena;
  eputs ("arena["); eputs (ntoab (g_cells, 16, 0)); eputs ("]: "); write_ (v); eputs ("\n");
  gc_stats_ ("1");
}

int
main (int argc, char **argv, char **envp)
{
  setenv ("MES_ARENA", "100", 1);
  setenv ("MES_MAX_ARENA", "100", 1);
  gc_init ();
#if POINTER_CELLS
  cell_zero = g_cells;
#else
  cell_zero = 0;
#endif
  cell_nil = cell_zero + M2_CELL_SIZE;
  cell_f = cell_nil + M2_CELL_SIZE;
  g_symbols = cell_zero;
  g_symbol_max = cell_zero + (9 * M2_CELL_SIZE);
  g_ports = cell_zero;
  g_macros = cell_zero;
  g_stack = STACK_SIZE;
  M0 = cell_zero;

  test_empty ();

  return 0;
}
