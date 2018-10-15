#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

LIBC=${LIBC-c}

##moduledir=${moduledir-${datadir}${datadir:+/}module}

sh ${srcdest}build-aux/snarf.sh --mes

ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crt0
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crt1
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crti
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crtn
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc-mini
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libtcc1
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc+gnu
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libg
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libgetopt

LIBC= sh ${srcdest}build-aux/cc32-mes.sh scaffold/main
LIBC=c-mini sh ${srcdest}build-aux/cc32-mes.sh scaffold/hello
LIBC=c-mini sh ${srcdest}build-aux/cc32-mes.sh scaffold/argv
sh ${srcdest}build-aux/cc32-mes.sh scaffold/read
sh ${srcdest}build-aux/cc32-mes.sh scaffold/malloc
sh ${srcdest}build-aux/cc32-mes.sh scaffold/micro-mes
sh ${srcdest}build-aux/cc32-mes.sh scaffold/tiny-mes
sh ${srcdest}build-aux/cc32-mes.sh scaffold/mini-mes

sh ${srcdest}build-aux/cc32-mes.sh src/mes

if [ "$CC32" = "$TCC" ]; then
    cp src/mes.mes-tcc-out src/mes
else
    cp src/mes.mes-gcc-out src/mes
fi
