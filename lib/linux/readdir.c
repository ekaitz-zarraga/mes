/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright (C) 1991,92,93,94,95,96,97,99,2000 Free Software Foundation, Inc.
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

// Taken from GNU C Library 2.2.5

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <assert.h>

#include <dirstream.h>

int getdents (int filedes, char *buffer, size_t nbytes);

struct dirent *
readdir (DIR *dir)
{
  struct dirent *entry;
  int saved_errno = errno;

  do
    {
      if (dir->offset >= dir->size)
        {
          /* We've emptied out our buffer.  Refill it.  */
          size_t size = dir->allocation;
          ssize_t bytes = getdents (dir->fd, dir->data, size);
          if (bytes <= 0)
            {
              /* Don't modifiy errno when reaching EOF.  */
              if (bytes == 0)
                errno = saved_errno;
              entry = 0;
              break;
            }
          dir->size = (size_t) bytes;

          /* Reset the offset into the buffer.  */
          dir->offset = 0;
        }

      entry = (struct dirent *) &dir->data[dir->offset];

      dir->offset += entry->d_reclen;
      dir->filepos = entry->d_off;

      /* Skip deleted files.  */
    }
  while (entry->d_ino == 0);

  return entry;
}
