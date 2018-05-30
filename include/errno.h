/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __MES_ERRNO_H
#define __MES_ERRNO_H 1

#if __GNUC__ && POSIX
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_ERRNO_H
#include_next <errno.h>
#else // ! (__GNUC__ && POSIX)
int errno;
#define	ENOENT		 2	/* No such file or directory */
#define	EIO		 5	/* I/O error */
#define	EBADF		 9	/* Bad file number */
#define	ENOMEM		12	/* Out of memory */
#define	EEXIST		17	/* File exists */
#define	ENOTDIR		20	/* Not a directory */
#define	EISDIR		21	/* Is a directory */
#define	EINVAL		22	/* Invalid argument */
#define	EMFILE		24	/* Too many open files */
#define	EPIPE		32	/* Broken pipe */
#define	ERANGE		34	/* Math result not representable */

#define	ENAMETOOLONG	36	/* File name too long */
#define	ENOSYS		38	/* Invalid system call number */
#define	ELOOP		40	/* Too many symbolic links encountered */

#endif // ! (__GNUC__ && POSIX)

#endif // __MES_ERRNO_H
