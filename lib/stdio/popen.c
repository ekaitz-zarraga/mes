/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef SHELL_FILE_NAME
#define	SHELL_FILE_NAME "/bin/sh"
#endif
#ifndef SHELL_COMMAND_NAME
#define	SHELL_COMMAND_NAME "sh"
#endif

FILE *
popen (char const *command, char const *mode)
{
  if (!command || !mode || (*mode != 'r' && *mode != 'w'))
    {
      errno = EINVAL;
      return 0;
    }

  int pipedes[2];
  if (pipe (pipedes) < 0)
    return NULL;

  pid_t pid = fork ();
  if (pid == -1)
    {
      close (pipedes[0]);
      close (pipedes[1]);
      return 0;
    }
  else if (pid == 0)
    {
      // child
      int dup = (*mode == 'w'
                 ? dup2 (pipedes[STDIN], STDIN)
                 : dup2 (pipedes[STDOUT], STDOUT));
      if (dup < 0)
	_exit (127);

      close (pipedes[STDIN]);
      close (pipedes[STDOUT]);

      char const *argv[4];
      argv[0] = SHELL_COMMAND_NAME;
      argv[1] = "-c";
      argv[2] = command;
      argv[3] = 0;
      execve (SHELL_FILE_NAME, (char *const *) argv, environ);
      _exit (127);
    }

  FILE *stream;
  // parent
  if (*mode == 'r')
    {
      close (pipedes[STDOUT]);
      fcntl (pipedes[STDIN], F_SETFD, FD_CLOEXEC);
      stream = fdopen (pipedes[STDIN], mode);
    }
  else
    {
      close (pipedes[STDIN]);
      fcntl (pipedes[STDOUT], F_SETFD, FD_CLOEXEC);
      stream = fdopen (pipedes[STDOUT], mode);
    }

  if (!stream)
    {
      int save_errno = errno;
      kill (pid, SIGKILL);
      if (!stream)
        close (pipedes[*mode == 'r' ? STDOUT : STDIN]);
      else
        fclose (stream);
      waitpid (pid, (int *)0, 0);
      errno = save_errno;
      return 0;
    }

  int filedes = fileno (stream);
  // XXX misuse ungetc buffer for PID
  // XXX TODO: make proper FILE struct
  __ungetc_init ();
  __ungetc_set (filedes, pid);
  return stream;
}
