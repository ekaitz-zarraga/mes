/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
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

struct scm *
getpid_ ()
{
  return make_number (getpid ());
}

struct scm *
environ_ (struct scm *args)
{
  /* No arguments; return the current environment. */
  if (args == cell_nil)
    {
      char **p = environ;
      struct scm *acc = cell_nil;
      while (*p != NULL)
        {
          acc = cons (make_string0 (*p), acc);
          p++;
        }
      return reverse_x_ (acc, cell_nil);
    }
  /* One argument; set the current environment. */
  else if (args->cdr == cell_nil)
    {
      /* This approach assumes that 'environ' always has enough space to
         store more variables ('setenv' makes the same assumption).  It
         also leaks memory, since it allocates environment entries with
         no intention of cleaning them up. */
      struct scm *env;

      /* Before we do anything, verify that the argument type is
         correct.  This way we can error out before modifying the
         current environment. */
      env = args->car;
      while (env->type == TPAIR)
        {
          if (env->car->type != TSTRING)
            error (cell_symbol_wrong_type_arg,
                   cons (args, cstring_to_symbol ("environ")));
          env = env->cdr;
        }
      if (env != cell_nil)
        error (cell_symbol_wrong_type_arg,
               cons (args, cstring_to_symbol ("environ")));

      /* Now that we know we have a list of strings, we can update the
         value of 'environ'. */
      char **p = environ;
      env = args->car;
      while (env->type == TPAIR)
        {
          *p = malloc (env->car->length + 1);
          strcpy (*p, cell_bytes (env->car->string));
          p++;
          env = env->cdr;
        }
      *p = NULL;
      return cell_unspecified;
    }
  /* More than one argument; error out. */
  else
    {
      error (cell_symbol_wrong_number_of_args,
             cons (args, cstring_to_symbol ("environ")));
    }
}
