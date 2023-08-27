/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2022,2023 Timothy Sample <samplet@ngyro.com>
 * Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
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
#include <time.h>

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
  struct scm *result;

  if (file_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (file_name, cstring_to_symbol ("opendir")));

  DIR *dirstream = opendir (cell_bytes (file_name->string));

  if (dirstream == NULL)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not open directory"), file_name));

  /* Oof.  Mes has no support for foreign objects, so we return this as
     bytevector with the pointer as its contents. */
  result = make_bytes (sizeof (DIR *));
  char *p = cell_bytes (result);
  memcpy (p, (char *) &dirstream, sizeof (DIR *));

  return result;
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

struct scm *
seek (struct scm *port, struct scm *offset, struct scm *whence)
{
  if (port->type != TNUMBER)
    error (cell_symbol_wrong_type_arg,
           cons (port, cstring_to_symbol ("seek")));
  off_t result = lseek (port->value, offset->value, whence->value);
  if (result == -1)
    error (cell_symbol_system_error,
           cons (make_string0 ("Error seeking"), port));
  return make_number (result);
}

struct scm *
chdir_ (struct scm *file_name)
{
  if (file_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (file_name, cstring_to_symbol ("chdir_")));

  if (chdir (cell_bytes (file_name->string)) != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not change directory"), file_name));

  return cell_unspecified;
}

struct scm *
statbuf_to_vector (struct stat *buf)
{
  struct scm *v = make_vector_ (18, cell_unspecified);
  vector_set_x_ (v, 0, make_number (buf->st_dev));
  vector_set_x_ (v, 1, make_number (buf->st_ino));
  vector_set_x_ (v, 2, make_number (buf->st_mode));
  vector_set_x_ (v, 3, make_number (buf->st_nlink));
  vector_set_x_ (v, 4, make_number (buf->st_uid));
  vector_set_x_ (v, 5, make_number (buf->st_gid));
  vector_set_x_ (v, 6, make_number (buf->st_rdev));
  vector_set_x_ (v, 7, make_number (buf->st_size));
  vector_set_x_ (v, 8, make_number (buf->st_atime));
  vector_set_x_ (v, 9, make_number (buf->st_mtime));
  vector_set_x_ (v, 10, make_number (buf->st_ctime));

  if (S_ISDIR (buf->st_mode))
    vector_set_x_ (v, 13, cstring_to_symbol ("directory"));
  else if (S_ISREG (buf->st_mode))
    vector_set_x_ (v, 13, cstring_to_symbol ("regular"));
  else if (S_ISFIFO (buf->st_mode))
    vector_set_x_ (v, 13, cstring_to_symbol ("fifo"));
  else
    vector_set_x_ (v, 13, cstring_to_symbol ("unknown"));

  vector_set_x_ (v, 14, make_number ((~S_IFMT) & buf->st_mode));

  /* FIXME: These are the nanosecond components of the "atime", "mtime",
     and "ctime" respectively.  They should be filled from 'buf', but we
     need to check if the kernel gave us microseconds or nanoseconds to
     do so. */
  vector_set_x_ (v, 15, make_number (0));
  vector_set_x_ (v, 16, make_number (0));
  vector_set_x_ (v, 17, make_number (0));

  return v;
}

struct scm *
stat__ (char const *variant, struct scm *args)
{
  int result;
  struct stat buf;
  struct scm *file_name;
  struct scm *exception_on_error = cell_t;

  if (args->type != TPAIR)
    error (cell_symbol_wrong_number_of_args,
           cstring_to_symbol (variant));

  file_name = args->car;

  if (file_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (file_name, cstring_to_symbol (variant)));

  if (args->cdr->type == TPAIR)
    {
      exception_on_error = args->cdr->car;
      if (args->cdr->cdr != cell_nil)
        error (cell_symbol_wrong_number_of_args,
               cstring_to_symbol (variant));
    }

  /* It's a little sneaky.... */
  if (variant[0] == 'l')
    result = lstat (cell_bytes (file_name->string), &buf);
  else
    result = stat (cell_bytes (file_name->string), &buf);

  if (result != 0)
    {
      if (exception_on_error != cell_f)
        error (cell_symbol_system_error,
               cons (make_string0 ("Could not read file attributes"),
                     file_name));
      else
        return cell_f;
    }

  return statbuf_to_vector (&buf);
}

struct scm *
stat_ (struct scm *args)
{
  return stat__ ("stat", args);
}

struct scm *
lstat_ (struct scm *args)
{
  return stat__ ("lstat", args);
}

struct scm *
rename_file (struct scm *old_name, struct scm *new_name)
{
  int result;

  if (old_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (old_name, cstring_to_symbol ("rename-file")));

  if (new_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (new_name, cstring_to_symbol ("rename-file")));

  result = rename (cell_bytes (old_name->string),
                   cell_bytes (new_name->string));

  if (result != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not rename file"),
                 cons (old_name, new_name)));

  return cell_unspecified;
}

/* TODO: Support an optional 'mode' argument. */
struct scm *
mkdir_ (struct scm *file_name)
{
  if (file_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (file_name, cstring_to_symbol ("mkdir")));

  if (mkdir (cell_bytes (file_name->string), 0777) != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not make directory"),
                 file_name));

  return cell_unspecified;
}

struct scm *
rmdir_ (struct scm *file_name)
{
  if (file_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (file_name, cstring_to_symbol ("rmdir")));

  if (rmdir (cell_bytes (file_name->string)) != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not remove directory"),
                 file_name));

  return cell_unspecified;
}

struct scm *
link_ (struct scm *old_name, struct scm *new_name)
{
  if (old_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (old_name, cstring_to_symbol ("link")));
  if (new_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (new_name, cstring_to_symbol ("link")));
  if (link (cell_bytes (old_name->string),
            cell_bytes (new_name->string)) != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not create (hard) link"),
                 new_name));
  return cell_unspecified;
}

struct scm *
symlink_ (struct scm *old_name, struct scm *new_name)
{
  if (old_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (old_name, cstring_to_symbol ("symlink")));
  if (new_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (new_name, cstring_to_symbol ("symlink")));
  if (symlink (cell_bytes (old_name->string),
               cell_bytes (new_name->string)) != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not create symbolic link"),
                 new_name));
  return cell_unspecified;
}

struct scm *
umask_ (struct scm *args)
{
  mode_t mask;
  if (args->type == TPAIR)
    {
      if (args->car->type != TNUMBER)
        error (cell_symbol_wrong_type_arg,
               cons (args->car, cstring_to_symbol ("umask")));
      if (args->cdr != cell_nil)
        error (cell_symbol_wrong_number_of_args,
               cstring_to_symbol ("umask"));
      return make_number (umask (args->car->value));
    }
  else
    {
      mask = umask (0);
      umask (mask);
      return make_number (mask);
    }
}

struct scm *
utime_ (struct scm *file_name, struct scm *actime, struct scm *modtime)
{
  struct timespec times[2];

  if (file_name->type != TSTRING)
    error (cell_symbol_wrong_type_arg,
           cons (file_name, cstring_to_symbol ("utime")));
  if (actime->type != TNUMBER)
    error (cell_symbol_wrong_type_arg,
           cons (actime, cstring_to_symbol ("utime")));
  if (modtime->type != TNUMBER)
    error (cell_symbol_wrong_type_arg,
           cons (modtime, cstring_to_symbol ("utime")));

  times[0].tv_sec = actime->value;
  times[0].tv_nsec = 0;
  times[1].tv_sec = modtime->value;
  times[1].tv_nsec = 0;

  if (utimensat (AT_FDCWD, cell_bytes (file_name->string), times, 0) != 0)
    error (cell_symbol_system_error,
           cons (make_string0 ("Could not update timestamps"),
                 file_name));

  return cell_unspecified;
}

struct scm *
sleep_ (struct scm *seconds)
{
  if (seconds->type != TNUMBER)
    error (cell_symbol_wrong_type_arg,
           cons (seconds, cstring_to_symbol ("sleep")));
  struct timespec requested_time;
  struct timespec remaining;
  requested_time.tv_sec = seconds->value;
  requested_time.tv_nsec = 0;
  if (nanosleep (&requested_time, &remaining) != 0)
    return make_number (remaining.tv_sec);
  else
    return make_number (0);
}
