#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

if test "$V" = 2; then
    set -x
fi

set -u
srcdest=${srcdest-}
. ${srcdest}build-aux/configure-lib.sh

mkdir -p $mes_cpu-mes
cp ${srcdest}lib/$mes_kernel/$mes_cpu-mes-$compiler/crt*.c $mes_cpu-mes

# Needs special treatment, in RISC-V 64 we have a separate lib for tcc, so we
# might need to build it too, instead of just leaving the `libc+gnu.c`
# generated from it.
# If we build both, we will be able to `REBUILD_LIBC` in the future, with GCC.

rm -f libc+gnu.c
cat > libc+gnu.c <<EOF
// Generated from Mes -- do not edit
// compiler: $compiler
// cpu:      $mes_cpu
// bits:     $mes_bits
// libc:     $mes_libc
// kernel:   $mes_kernel
// system:   $mes_system

EOF
for c in $libc_gnu_SOURCES; do
    echo "// $c" >> libc+gnu.c
    cat ${srcdest}$c >> libc+gnu.c
    echo >> libc+gnu.c
done
cp libc+gnu.c $mes_cpu-mes

rm -f libtcc1.c
cat > libtcc1.c <<EOF
// Generated from Mes -- do not edit
// compiler: $compiler
// cpu:      $mes_cpu
// bits:     $mes_bits
// libc:     $mes_libc
// kernel:   $mes_kernel
// system:   $mes_system

EOF
for c in $libtcc1_SOURCES; do
    echo "// $c" >> libtcc1.c
    cat ${srcdest}$c >> libtcc1.c
    echo >> libtcc1.c
done
cp libtcc1.c $mes_cpu-mes

cp ${srcdest}lib/posix/getopt.c $mes_cpu-mes/libgetopt.c
