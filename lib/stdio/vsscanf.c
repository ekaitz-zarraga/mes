/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
vsscanf (char const *s, char const *template, va_list ap)
{
  char *p = (char *) s;
  char const *t = template;
  int count = 0;
  while (*t && *p)
    if (*t != '%')
      {
        t++;
        p++;
      }
    else
      {
        t++;
        char c = *t;
        int skip_p = 0;
        int length = -1;
        if (c == '*')
          skip_p = 1;
        if (c >= '0' && c <= '9')
          {
            length = abtol (&t, 10);
            c = *t;
          }
        if (c == 'l')
          c = *++t;
        switch (c)
          {
          case '%':
            {
              p++;
              break;
            }
          case 'c':
            {
              char r = *p++;
              if (!skip_p)
                {
                  char *c = va_arg (ap, char *);
                  *c = r;
                  count++;
                }
              break;
            }
          case 'd':
          case 'i':
          case 'u':
            {
              if (skip_p)
                abtol ((char const **) &p, 10);
              else
                {
                  int *d = va_arg (ap, int *);
                  *d = abtol ((char const **) &p, 10);
                  count++;
                }
              break;
            }
          case 'e':
          case 'f':
          case 'g':
          case 'E':
          case 'G':
            {
              if (skip_p)
                strtod (p, &p);
              else
                {
                  float *f = va_arg (ap, float *);
                  *f = strtod (p, &p);
                  count++;
                }
              break;
            }
          case 's':
            {
              char *s = skip_p ? 0 : va_arg (ap, char *);
              char r = *p;
              while (r && !isspace (r) && (length == -1 || length--))
                {
                  if (!skip_p)
                    *s++ = r;
                  r = *++p;
                }
              if (!skip_p)
                {
                  count++;
                  *s = 0;
                }
              break;
            }
          case '[':
            {
              char *s = skip_p ? 0 : va_arg (ap, char *);
              char set[1024];
              int i = 0;
              int not_in_set_p = 0;
              t++;
              if (*t == '^')
                {
                  not_in_set_p = 1;
                  t++;
                }
              if (*t == ']' || *t == '-')
                set[i++] = *t++;
              while (*t && *t != ']')
                {
                  if (*t == '-')
                    {
                      char end = *t++;
                      for (char x = set[i - 1] + 1; x < end; x++)
                        set[i++] = x;
                    }
                  else
                    set[i++] = *t++;
                }
              set[i] = 0;
              char r = *p;
              while (r && (length == -1 || length--))
                {
                  int match = (int) (long) strchr (set, r);
                  if (not_in_set_p)
                    match = !match;
                  if (!match)
                    break;
                  if (!skip_p)
                    *s++ = r;
                  r = *++p;
                }
              if (!skip_p)
                {
                  count++;
                  *s = 0;
                }
              break;
            }
          default:
            {
              eputs ("vsscanf: not supported: %:");
              eputc (c);
              eputs (", in template: ");
              eputs (template);
              eputs ("\n");
              t++;
              p++;
            }
          }
        t++;
      }
  va_end (ap);
  return count;
}
