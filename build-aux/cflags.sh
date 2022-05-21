# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018,2019,2020,2022 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

case "$mes_cpu" in
    arm)
        stage0_arch=40
        stage0_cpu=armv7l
        ;;
    x86_64)
        stage0_arch=2
        stage0_cpu=amd64
        ;;
    x86)
        stage0_arch=1
        stage0_cpu=x86
        ;;
    *)
        stage0_arch=1
        stage0_cpu=$mes_cpu
        ;;
esac

AM_CPPFLAGS="
-D HAVE_CONFIG_H=1
-I ${srcdest}include
-I ${srcdest}include/$mes_kernel/$mes_cpu
"

if test $mes_kernel = gnu; then
    AM_CPPFLAGS="$AM_CPPFLAGS
-I /usr/include
"
fi

AM_CFLAGS=
if test "$V" = 2; then
    AM_CFLAGS="$AM_CFLAGS -v"
elif test "$V" = 3; then
    AM_CFLAGS="$AM_CFLAGS -v -v"
fi

if test $mes_libc = mes; then
    AM_CFLAGS="$AM_CFLAGS
-static
-nostdinc
-nostdlib
-fno-builtin
"
fi

AM_LDFLAGS="
-L .
"
if test $mes_libc = mes; then
    AM_LDFLAGS="$AM_LDFLAGS
-static
-nostdlib
"
fi
LIBS=-lc

if test $mes_cpu = arm; then
    AM_CFLAGS="$AM_CFLAGS
-marm
"
fi

export AM_CFLAGS CFLAGS
export AM_CPPFLAGS CPPFLAGS
export AM_LDFLAGS LDFLAGS
export LIBS
