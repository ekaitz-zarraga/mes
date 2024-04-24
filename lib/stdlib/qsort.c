/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 Paul Dersey <pdersey@gmail.com>
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

/* See: https://en.wikipedia.org/wiki/Quicksort */

#include <stdlib.h>

static void
qswap (char *a, char *b, size_t size)
{
  while (size-- > 0)
    {
      char tmp = *a;
      *a++ = *b;
      *b++ = tmp;
    }
}

static size_t
qpart (char *base, size_t lo, size_t hi, size_t size,
       int (*compare) (void const *, void const *))
{
  char *pivot = base + hi * size;
  size_t i = lo;

  for (size_t j = lo; j < hi; j++)
    {
      char *pj = base + j * size;
      if (pj != pivot && compare (pj, pivot) < 0)
        {
          void *pi = base + i * size;
          qswap (pi, pj, size);
          i++;
        }
    }

  char *pi = base + i * size;
  qswap (pi, pivot, size);

  return i;
}

static void
_qsort (void *base, size_t lo, size_t hi, size_t size,
        int (*compare) (void const *, void const *))
{
  if (lo >= hi)
    return;
  size_t pi = qpart (base, lo, hi, size, compare);
  if (pi > 0)
    _qsort (base, lo, pi - 1, size, compare);
  _qsort (base, pi + 1, hi, size, compare);
}

void
qsort (void *base, size_t count, size_t size,
       int (*compare) (void const *, void const *))
{
  if (count <= 1)
    return;
  _qsort (base, 0, count - 1, size, compare);
}
