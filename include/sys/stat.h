/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2022 Dor Askayo <dor.askayo@gmail.com>
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
#ifndef __MES_SYS_STAT_H
#define __MES_SYS_STAT_H 1lei

#if SYSTEM_LIBC
#undef __MES_SYS_STAT_H
#include_next <sys/stat.h>

#else // ! SYSTEM_LIBC

#include <time.h>
#include <sys/types.h>
#include <syscall.h>

#ifndef __MES_MODE_T
#define __MES_MODE_T
typedef int mode_t;
#endif

// *INDENT-OFF*
#if __M2__
struct stat
{
  unsigned st_dev;
  unsigned st_ino;
  unsigned st_mode;
  unsigned st_nlink;
  unsigned st_uid;
  unsigned st_gid;
  unsigned st_rdev;
  long     st_size; /* Linux: unsigned long; glibc: off_t (i.e. signed) */
  unsigned st_blksize;
  unsigned st_blocks;
  time_t   st_atime; /* Linux: unsigned long; glibc: time_t */
  unsigned st_atime_usec;
  time_t   st_mtime; /* Linux: unsigned long; glibc: time_t */
  unsigned st_mtime_usec;
  time_t   st_ctime; /* Linux: unsigned long; glibc: time_t */
  unsigned st_ctime_usec;
  unsigned __foo0;
  unsigned __foo1;
};
// FIXME: M2-Planet 1.10.0 crashes on this...
// #elif (__i386__ || __arm__) && !SYS_stat64
#define __i386__or__arm__ (__i386__or__arm__)
#elif __i386__or__arm__ && !SYS_stat64
struct stat
{
  unsigned long  st_dev;
  unsigned long  st_ino;
  unsigned short st_mode;
  unsigned short st_nlink;
  unsigned short st_uid;
  unsigned short st_gid;
  unsigned long  st_rdev;
  long           st_size; /* Linux: unsigned long; glibc: off_t (i.e. signed) */
  unsigned long  st_blksize;
  unsigned long  st_blocks;
  time_t         st_atime; /* Linux: unsigned long; glibc: time_t */
  unsigned long  st_atime_usec;
  time_t         st_mtime; /* Linux: unsigned long; glibc: time_t */
  unsigned long  st_mtime_usec;
  time_t         st_ctime; /* Linux: unsigned long; glibc: time_t */
  unsigned long  st_ctime_usec;
  unsigned long  __foo0;
  unsigned long  __foo1;
};
#else
struct stat
{
  unsigned long  st_dev;
  unsigned long  st_ino;
  unsigned int   st_mode;
  unsigned int   st_nlink;
  unsigned int   st_uid;
  unsigned int   st_gid;
  unsigned long  st_rdev;
  long           st_size;
  unsigned long  st_blksize;
  unsigned long  st_blocks;
  time_t         st_atime;
  unsigned long  st_atime_usec;
  time_t         st_mtime;
  unsigned long  st_mtime_usec;
  time_t         st_ctime;
  unsigned long  st_ctime_usec;
  unsigned long  __foo0;
  unsigned long  __foo1;
};
#endif
// *INDENT-ON*

int chmod (char const *file_name, mode_t mode);
int fstat (int filedes, struct stat *buf);
int mkdir (char const *file_name, mode_t mode);
int mknod (char const *file_name, mode_t mode, dev_t dev);
int chown (char const *file_name, uid_t owner, gid_t group);
int rmdir (char const *file_name);
int stat (char const *file_name, struct stat *buf);

#define S_IFIFO 0010000
#define S_IFCHR 0020000
#define S_IFDIR 0040000
#define S_IFBLK 0060000
#define S_IFREG 0100000
#define S_IFLNK 0120000
#define S_IFMT  0170000

#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)

#define S_IRWXU 00700
#define S_IXUSR 00100
#define S_IWUSR 00200
#define S_IRUSR 00400

#define S_ISUID 04000
#define S_ISGID 02000
#define S_IXGRP 00010
#define S_IXOTH 00001
#define S_IRGRP 00040
#define S_IROTH 00004
#define S_IWGRP 00020
#define S_IWOTH 00002
#define S_IRWXG 00070
#define S_IRWXO 00007

#endif // ! SYSTEM_LIBC

#endif // __MES_SYS_STAT_H
