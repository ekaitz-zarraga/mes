/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2022,2023 Timothy Sample <samplet@ngyro.com>
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

#include <errno.h>
#include <dirent.h>

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

struct scm *
opendir_ (struct scm *file_name)
{
  if (file_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (file_name, cstring_to_symbol ("opendir")));

  DIR *dirstream = opendir (cell_bytes (file_name->string));

  if (dirstream == NULL)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not open directory"), file_name));

  /* Oof.  Mes has no support for foreign objects, so we return this as
     bytevector with the pointer as its contents. */
  return make_bytes ((char *) &dirstream, sizeof (DIR *));
}

struct scm *
closedir_ (struct scm *dir)
{
  if (dir->type != TBYTES || dir->length != sizeof (DIR *))
    error (cell_symbol_wrong_type_arg,
           cons (dir, cstring_to_symbol ("closedir")));

  char *bytes = cell_bytes (dir);
  DIR *dirstream = *((DIR **) bytes);

  int result = closedir (dirstream);

  if (result != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Error closing directory"), dir));

  for (long i = 0; i < dir->length; i++)
      bytes[i] = 0;

  return cell_unspecified;
}

struct scm *
readdir_ (struct scm *dir)
{
  if (dir->type != TBYTES || dir->length != sizeof (DIR *))
    error (cell_symbol_wrong_type_arg,
           cons (dir, cstring_to_symbol ("readdir")));

  char *bytes = cell_bytes (dir);
  DIR *dirstream = *((DIR **) bytes);

  errno = 0;
  struct dirent *dent = readdir (dirstream);

  if (errno != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Error reading from directory"), dir));

  if (dent != NULL)
    return make_string0 (dent->d_name);
  else
    return cell_f;
}

struct scm *
pipe_ ()
{
  int fds[2];
  if (pipe (fds) != 0)
    error (cell_symbol_system_error, make_string0 ("Could not create pipe"));
  return cons (make_number (fds[0]), make_number (fds[1]));
}

struct scm *
close_port (struct scm *port)
{
  if (port->type == TNUMBER)
    if (close (port->value) != 0)
      error (cell_symbol_system_error, cons (make_string0 ("Error closing port"), port));
}
