/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019,2023 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib-mini.h>
int main (int argc, char *argv[], char *envp[]);

// *INDENT-OFF*
void
_start ()
{
  asm (
       "mov     %ebp,%eax\n\t"
       "add     $4,%eax\n\t"
       "mov     (%eax),%eax\n\t"
       "add     $3,%eax\n\t"
       "shl     $2,%eax\n\t"
       "add     %ebp,%eax\n\t"
       "push    %eax\n\t"

       "mov     %ebp,%eax\n\t"
       "add     $8,%eax\n\t"
       "push    %eax\n\t"

       "mov     %ebp,%eax\n\t"
       "add     $4,%eax\n\t"
       "mov     (%eax),%eax\n\t"
       "push    %eax\n\t"

       "call    __init_io\n\t"
       "call    main\n\t"

       "mov     %eax,%ebx\n\t"
       "mov     $1,%eax\n\t"
       "int     $0x80\n\t"
       "hlt     \n\t"
       );
}
