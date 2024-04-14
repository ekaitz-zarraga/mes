#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018,2019,2022,2023,2024 Janneke Nieuwenhuizen <janneke@gnu.org>
# Copyright © 2024 Michael Forney <mforney@mforney.org>
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
set -u

V=${V-1}

if [ "$V" = 2 ]; then
    set -x
fi

. ./config.sh
. ${srcdest}build-aux/trace.sh
. ${srcdest}build-aux/cflags.sh

if $courageous; then
    set +e
    set_min_e () {
        echo "Applying courage"
    }
    set_min_e
else
    set_min_e () {
        set -e
    }
fi

trace "CCLD       ${srcdest}lib/$mes_kernel/$mes_cpu-mes-$compiler/exit-42.S" $CC $LDFLAGS \
      -nostdlib                                                                            \
      -g                                                                                   \
      ${srcdest}lib/$mes_kernel/$mes_cpu-mes-$compiler/exit-42.S                           \
      -o exit-42

trace "TEST       exit-42"
{ set +e; ./exit-42; r=$?; set_min_e; }
[ $r != 42 ] && echo "  => $r"
[ $r = 42 ]

trace "HEX2       ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-0exit-42.hex2" $HEX2 \
      --little-endian                                                                      \
      --architecture $stage0_cpu                                                           \
      --base-address 0x1000000                                                             \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-0header.hex2                  \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-0exit-42.hex2                 \
      -o 0exit-42

trace "TEST       0exit-42"
{ set +e; ./0exit-42; r=$?; set_min_e; }
[ $r != 42 ] && echo "  => $r"
[ $r = 42 ]

trace "HEX2       ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-body-exit-42.hex2" $HEX2  \
      --little-endian                                                                           \
      --architecture $stage0_cpu                                                                \
      --base-address 0x1000000                                                                  \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-header.hex2                        \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-body-exit-42.hex2                  \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-footer-single-main.hex2            \
      -o body-exit-42

trace "TEST       body-exit-42"
{ set +e; ./body-exit-42; r=$?; set_min_e; }
[ $r != 42 ] && echo "  => $r"
[ $r = 42 ]

### FIXME: c&p from exit-42
trace "CCLD       ${srcdest}lib/$mes_kernel/$mes_cpu-mes-$compiler/hello-mes.S" $CC $LDFLAGS    \
      -nostdlib                                                                                 \
      -g                                                                                        \
      ${srcdest}lib/$mes_kernel/$mes_cpu-mes-$compiler/hello-mes.S                              \
      -o hello-mes

trace "TEST       hello-mes"
{ set +e; ./hello-mes; r=$?; set_min_e; }
[ $r != 0 ] && echo "  => $r"
[ $r = 0 ]

trace "HEX2       ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-0hello-mes.hex2" $HEX2    \
      --little-endian                                                                           \
      --architecture $stage0_cpu                                                                \
      --base-address 0x1000000                                                                  \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-0header.hex2                       \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-0hello-mes.hex2                    \
      -o 0hello-mes

trace "TEST       0hello-mes"
{ set +e; ./0hello-mes; r=$?; set_min_e; }
[ $r != 0 ] && echo "  => $r"
[ $r = 0 ]

trace "HEX2       ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-body-hello-mes.hex2" $HEX2        \
      --little-endian                                                                                   \
      --architecture $stage0_cpu                                                                        \
      --base-address 0x1000000                                                                          \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-header.hex2                                \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-body-hello-mes.hex2                        \
      -f ${srcdest}lib/$mes_kernel/$mes_cpu-mes/elf$mes_bits-footer-single-main.hex2                    \
      -o body-hello-mes

trace "TEST       body-hello-mes"
{ set +e; ./body-hello-mes; r=$?; set_min_e; }
[ $r != 0 ] && echo "  => $r"
[ $r = 0 ]
