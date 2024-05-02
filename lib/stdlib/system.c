/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef SHELL_FILE_NAME
#define	SHELL_FILE_NAME "/bin/sh"
#endif
#ifndef SHELL_COMMAND_NAME
#define	SHELL_COMMAND_NAME "sh"
#endif

int
system (char const *command)
{
  pid_t pid = fork ();
  if (pid == -1)
    return -1;
  else if (pid == 0)
    {
      // child
      char const *argv[4];
      argv[0] = SHELL_COMMAND_NAME;
      argv[1] = "-c";
      argv[2] = command;
      argv[3] = 0;
      execve (SHELL_FILE_NAME, (char *const *) argv, environ);
      _exit (127);
    }

  // parent
  int status;
  pid_t child_pid = waitpid (pid, &status, 0);
  if (child_pid != pid)
    return -1;

  return status;
}
