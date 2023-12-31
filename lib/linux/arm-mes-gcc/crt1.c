/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019,2020,2023 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019,2020 Danny Milosavljevic <dannym@scratchpost.org>
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

/* Note: GCC automatically emits a preable in order to set up the
frame pointer: "push {fp}" "add fp, sp, 0"
 */
// *INDENT-OFF*
#if !__TINYC__
void
_start ()
{
  /* environ = argv + argc + 1 */
  asm (
       "ldr     r0,[fp,#4]\n\t" /* r0 = argc */
       "add     r1,fp,#8\n\t" /* r1 = &argv[0] */
       "add     r2,r0,#1\n\t" /* r2 = r0 + 1 */
       "lsl     r2,#2\n\t" /* r2 = (r0 + 1) << 2 */
       "add     r2,r2,r1\n\t" /* r2 = ((r0 + 1) << 2) + r1 */
       "push    {r2}\n\t" /* envp */
       "push    {r1}\n\t" /* argv */
       "push    {r0}\n\t" /* argc */
       "ldr     r0,[sp]\n\t" /* argc */
       "ldr     r1,[sp, #4]\n\t" /* argv */
       "ldr     r2,[sp, #8]\n\t" /* envp */
       "bl      __init_io\n\t"
       "bl      main\n\t"
       "mov     r7, #1\n\t"
       "swi     #0\n\t"
       "wfi     \n\t"
       );
}
#else //__TINYC__
void
_start ()
{
  /* environ = argv + argc + 1 */
  __asm__ (".int 0xe59b000c\n"); //ldr   r0, [fp, #12]
  __asm__ (".int 0xe28b1010\n"); //add   r1, fp, #16

  __asm__ (".int 0xe2802001\n"); //add   r2, r0, #1
  __asm__ (".int 0xe1a02102\n"); //lsl   r2, r2, #2
  __asm__ (".int 0xe0822001\n"); //add   r2, r2, r1

  // setup argc, argv, envp parameters on stack
  __asm__ (".int 0xe52d2004\n"); //push  {r2}      ; (str r2, [sp, #-4]!)
  __asm__ (".int 0xe52d1004\n"); //push  {r1}      ; (str r1, [sp, #-4]!)
  __asm__ (".int 0xe52d0004\n"); //push  {r0}      ; (str r0, [sp, #-4]!)
  __asm__ (".int 0xe1a02002\n"); //mov   r2, r2

  // setup argc, argv, envp parameters in registers
  __asm__ (".int 0xe59d0000\n"); //ldr   r0, [sp]
  __asm__ (".int 0xe59d1004\n"); //ldr   r1, [sp, #4]
  __asm__ (".int 0xe59d2008\n"); //ldr   r2, [sp, #8]
  __init_io ();
  main ();

  __asm__ (".int 0xe3a07001\n"); //mov   r7, #1
  __asm__ (".int 0xef000000\n"); //svc   0x00000000
  __asm__ (".int 0xe320f003\n"); //wfi
  __asm__ (".int 0xe320f000\n"); //nop   {0}
}
#endif //__TINYC__
