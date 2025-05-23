/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020,2021,2023 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
 * Copyright © 2022 Timothy Sample <samplet@ngyro.com>
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

#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/utsname.h>
#include <time.h>

struct scm *
mes_g_stack (struct scm *a)             /*:((internal)) */
{
  g_stack = STACK_SIZE;
  R0 = a;
  R1 = make_char (0);
  R2 = make_char (0);
  R3 = make_char (0);
  return R0;
}

struct scm *
mes_environment (int argc, char **argv)
{
  struct scm *a = init_symbols ();

  /* These are symbols that the evaluator treats specially.  We bind
     them to themselves so that they can be looked up and bound at
     expansion time. */
  a = acons (cell_symbol_call_with_current_continuation,
             cell_symbol_call_with_current_continuation, a);
  a = acons (cell_symbol_call_with_values,
             cell_symbol_call_with_values, a);
  a = acons (cell_symbol_current_environment,
             cell_symbol_current_environment, a);
  a = acons (cell_symbol_lambda, cell_symbol_lambda, a);
  a = acons (cell_symbol_quote, cell_symbol_quote, a);
  a = acons (cell_symbol_begin, cell_symbol_begin, a);
  a = acons (cell_symbol_if, cell_symbol_if, a);
  a = acons (cell_symbol_set_x, cell_symbol_set_x, a);
  a = acons (cell_symbol_define, cell_symbol_define, a);
  a = acons (cell_symbol_define_macro, cell_symbol_define_macro, a);

  char *compiler = "gnuc";
#if __MESC__
  compiler = "mesc";
#elif __M2__
  compiler = "m2c";
#elif __TINYC__
  compiler = "tcc";
#endif
  a = acons (cell_symbol_compiler, make_string0 (compiler), a);

  char *arch;
#if __i386__
  arch = "x86";
#elif __arm__
  arch = "arm";
#elif __x86_64__
  arch = "x86_64";
#elif __riscv_xlen == 32
  arch = "riscv32";
#elif __riscv_xlen == 64
  arch = "riscv64";
#else
#error arch not supported
#endif
  a = acons (cell_symbol_arch, make_string0 (arch), a);

#if !MES_MINI
  struct scm *lst = cell_nil;
  int i;
  for (i = argc - 1; i >= 0; i = i - 1)
    lst = cons (make_string0 (argv[i]), lst);
  a = acons (cell_symbol_argv, lst, a);
#endif

  return mes_g_stack (a);
}

int
try_open_boot (char *file_name, char const *boot, char const *location)
{
  strcpy (file_name + strlen (file_name), boot);
  if (g_debug > 1)
    {
      eputs ("mes: reading boot [");
      eputs (location);
      eputs ("]: ");
      eputs (file_name);
      eputs ("\n");
    }
  int fd = mes_open (file_name, O_RDONLY, 0);
  if (g_debug != 0 && fd > 0)
    {
      eputs ("mes: read boot: ");
      eputs (file_name);
      eputs ("\n");
    }
  return fd;
}

void
open_boot ()
{
  __stdin = -1;
  char *boot = __open_boot_buf;
  char *file_name = __open_boot_file_name;
  strcpy (g_datadir, ".");
  if (getenv ("MES_BOOT") != 0)
    strcpy (boot, getenv ("MES_BOOT"));
  else
    strcpy (boot, "boot-5.scm");
  if (getenv ("MES_PREFIX") != 0)
    {
      strcpy (g_datadir, getenv ("MES_PREFIX"));
      strcpy (g_datadir + strlen (g_datadir), "/mes");
      strcpy (file_name, g_datadir);
      strcpy (file_name + strlen (file_name), "/module/mes/");
      __stdin = try_open_boot (file_name, boot, "MES_PREFIX");
      if (__stdin < 0)
        {
          strcpy (g_datadir, getenv ("MES_PREFIX"));
          strcpy (g_datadir + strlen (g_datadir), "/share/mes");
          strcpy (file_name, g_datadir);
          strcpy (file_name + strlen (file_name), "/module/mes/");
          __stdin = try_open_boot (file_name, boot, "MES_PREFIX/share/mes");
        }
    }
  if (__stdin < 0)
    {
      g_datadir[0] = 0;
      if (getenv ("srcdest") != 0)
        strcpy (g_datadir, getenv ("srcdest"));
      strcpy (g_datadir + strlen (g_datadir), "mes");
      strcpy (file_name, g_datadir);
      strcpy (file_name + strlen (file_name), "/module/mes/");
      __stdin = try_open_boot (file_name, boot, "${srcdest}mes");
    }
  if (__stdin < 0)
    {
      file_name[0] = 0;
      __stdin = try_open_boot (file_name, boot, "<boot>");
    }
  if (__stdin < 0)
    {
      eputs ("mes: boot failed: no such file: ");
      eputs (boot);
      eputs ("\n");
      exit (1);
    }
}

struct scm *
read_boot ()                    /*:((internal)) */
{
  struct scm *program = read_input_file_env (cell_nil);
  __stdin = STDIN;
  return program;
}

void
init ()
{
  __execl_c_argv = malloc (1024 * sizeof (char *));     /* POSIX minimum: 4096 */
  __execle_c_env = malloc (1024 * sizeof (char *));
  __gettimeofday_time = malloc (sizeof (struct timeval));
  __get_internal_run_time_ts = malloc (sizeof (struct timespec));
  __open_boot_buf = malloc (PATH_MAX);
  __open_boot_file_name = malloc (PATH_MAX);
  __reader_read_char_buf = malloc (10);
  __setenv_buf = malloc (1024);
  __uts = malloc (sizeof (struct utsname));
  g_datadir = malloc (1024);
  g_start_time = malloc (sizeof (struct timespec));
  memset (g_start_time, 0, sizeof (struct timespec));
  gc_start_time = malloc (sizeof (struct timespec));
  gc_end_time = malloc (sizeof (struct timespec));

  char *p = getenv ("MES_DEBUG");
  if (p != 0)
    g_debug = atoi (p);
  g_mini = cast_charp_to_long (getenv ("MES_MINI"));
  open_boot ();
  gc_init ();
}

int
main (int argc, char **argv)
{
  init ();

  struct scm *a = mes_environment (argc, argv);
  a = mes_builtins (a);
  a = init_time (a);
  M0 = make_initial_module (a);
  M1 = cell_f;
  g_macros = make_hash_table_ (0);

  if (g_debug > 5)
    {
      eputs ("initial module obarray\n");
      hash_table_printer (M0);
    }

  struct scm *program = read_boot ();
  R0 = acons (cell_symbol_program, program, cell_nil);
  R2 = program;
  push_cc (R2, cell_unspecified, R0, cell_unspecified);

  if (g_debug > 2)
    gc_stats_ ("\n gc boot");
  if (g_debug > 3)
    {
      eputs ("program: ");
      write_error_ (R1);
      eputs ("\n");
    }
  R3 = cell_vm_begin_expand;
  R1 = eval_apply ();
  if (g_debug != 0)
    {
      write_error_ (R1);
      eputs ("\n");
    }
  if (g_debug != 0)
    {
      if (g_debug > 5)
        {
          eputs ("initial module obarray\n");
          hash_table_printer (M0);
        }

      if (g_debug < 3)
        gc_stats_ ("\ngc run");
      MAX_ARENA_SIZE = 0;

      gc (g_stack);
      if (g_debug < 3)
        gc_stats_ (" => ");

      if (g_debug > 5)
        {
          eputs ("\nports:");
          write_error_ (g_ports);
          eputs ("\n");
        }
      eputs ("\n");
    }

  return 0;
}
