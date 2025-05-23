/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_SIGNAL_H
#define __MES_SIGNAL_H 1

#if SYSTEM_LIBC
#undef __MES_SIGNAL_H
#include_next <signal.h>
#else //! SYSTEM_LIBC

#define _NSIG 64

#define _SIGSET_NITEMS (_NSIG / (8 * sizeof(unsigned long)))

#if !__M2__
typedef struct
{
  unsigned long items[_SIGSET_NITEMS];
} sigset_t;
#endif
typedef long stack_t;

#include <sys/types.h>

// *INDENT-OFF*
#define NSIG 30
#define SIGHUP     1
#define SIGINT     2
#define SIGQUIT    3
#define SIGILL     4
#define SIGTRAP    5
#define SIGABRT    6
#define SIGIOT     6
#define SIGBUS     7
#define SIGFPE     8
#define SIGKILL    9
#define SIGUSR1   10
#define SIGSEGV   11
#define SIGUSR2   12
#define SIGPIPE   13
#define SIGALRM   14
#define SIGTERM   15
#define SIGSTKFLT 16
#define SIGCHLD   17
#define SIGCONT   18
#define SIGSTOP   19
#define SIGTSTP   20
#define SIGTTIN   21
#define SIGTTOU   22
#define SIGURG    23
#define SIGXCPU   24
#define SIGXFSZ   25
#define SIGVTALRM 26
#define SIGPROF   27
#define SIGWINCH  28
#define SIGIO     29
#define SIGPOLL   SIGIO

#define FPE_INTDIV 1
#define FPE_INTOVF 2
#define FPE_FLTDIV 3
#define FPE_FLTOVF 4
#define FPE_FLTUND 5
#define FPE_FLTRES 6
#define FPE_FLTINV 7
#define FPE_FLTSUB 8

#define SA_NOCLDSTOP 0x00000001
#define SA_NOCLDWAIT 0x00000002
#define SA_SIGINFO   0x00000004
#define SA_RESTORER  0x04000000
#define SA_ONSTACK   0x08000000
#define SA_RESTART   0x10000000
#define SA_NODEFER   0x40000000
#define SA_RESETHAND 0x80000000

#define SA_NOMASK  SA_NODEFER
#define SA_ONESHOT SA_RESETHAND

#if !__M2__ // lacks short, casts
typedef struct siginfo_t
{
  int          si_signo;
  int          si_errno;
  int          si_code;
  int          si_trapno;
  pid_t        si_pid;
  uid_t        si_uid;
  int          si_status;
  clock_t      si_utime;
  clock_t      si_stime;
  sigval_t     si_value;
  int          si_int;
  void        *si_ptr;
  int          si_overrun;
  int          si_timerid;
  void        *si_addr;
  long         si_band;
  int          si_fd;
  short        si_addr_lsb;
  void        *si_lower;
  void        *si_upper;
  int          si_pkey;
  void        *si_call_addr;
  int          si_syscall;
  unsigned int si_arch;
} siginfo_t;
// *INDENT-ON*

#if __M2__ || __MESC__
typedef long sighandler_t;
#else
typedef void (*sighandler_t) (int);
#endif

#if __i386__ || __x86_64__
struct sigaction
{
  union
  {
    sighandler_t sa_handler;
    void (*sa_sigaction) (int signum, siginfo_t *, void *);
  };
  unsigned long sa_flags;
#if __x86_64__
  long _foo0;
#endif
  sigset_t sa_mask;
#if __x86_64__
  long _foo1[15];
#endif
  //unsigned long sa_flags; // x86?
  void (*sa_restorer) (void);
};
#else /* uapi */
struct sigaction
{
  union
  {
    sighandler_t sa_handler;
    void (*sa_sigaction) (int signum, siginfo_t *, void *);
  };
  unsigned long sa_flags;
  void (*sa_restorer) (void);
  sigset_t sa_mask;
};
#endif

#define SIG_DFL ((sighandler_t)0)
#define SIG_IGN ((sighandler_t)1)
#define SIG_ERR ((sighandler_t)-1)

#include <arch/signal.h>

int kill (pid_t pid, int signum);
int raise (int);
int sigaction (int signum, struct sigaction const *act, struct sigaction *oldact);
int sigaddset (sigset_t * set, int signum);
#if __MESC__
void *signal (int signum, void *action);
#else
sighandler_t signal (int signum, sighandler_t action);
#endif
int sigemptyset (sigset_t * set);
#ifndef SIG_BLOCK
#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2
#endif
int sigprocmask (int how, sigset_t const *set, sigset_t * oldset);

#endif // !__M2__
#endif //! SYSTEM_LIBC

#endif // __MES_SIGNAL_H
