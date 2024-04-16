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

#include <assert.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

size_t file_size (char const* file_name)
{
  struct stat st;
  stat (file_name, &st);
  return st.st_size;
}

int
main (int argc, char const *argv[])
{
  eputs ("test:mmap\n");
#if __GNUC__ > 11
  char* file_name = malloc (PATH_MAX);
#else
  // FIXME: gcc-12.2.0 fails open using this
  char file_name[PATH_MAX];
#endif
  char *srcdir = getenv ("abs_top_srcdir");
  if (! srcdir) // for running by hand
    srcdir = ".";
  eputs ("srcdir=");
  eputs (srcdir);
  eputs ("\n");
  strcpy (file_name, srcdir);
  strcpy (file_name + strlen (srcdir), "/lib/tests/posix/data/open-read");
  eputs ("test mmap:");
  eputs (file_name);
  eputs ("\n");

  size_t size = file_size (file_name);
  int fd = open (file_name, O_RDONLY, 0);
  assert (fd != -1);

  void* data = mmap (NULL, size, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
  assert (data != MAP_FAILED);
  write (1, data, size);

  int rc = munmap (data, size);
  assert (rc == 0);
  close (fd);

  return 0;
}
