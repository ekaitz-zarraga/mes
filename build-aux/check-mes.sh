#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018,2023,2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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
if test -z "$config_sh"; then
    . ./config.sh
fi
if [ -n "$srcdest" ]; then
    GUILE_LOAD_PATH="${srcdest}module:${srcdest}mes:${srcdest}:$GUILE_LOAD_PATH"
fi
set -u

mes_tests="
tests/boot.test
tests/read.test
tests/srfi-0.test
tests/macro.test
tests/gc.test
tests/perform.test
tests/base.test
tests/quasiquote.test
tests/let.test
tests/closure.test
tests/scm.test
tests/display.test
tests/cwv.test
tests/math.test
tests/vector.test
tests/hash.test
tests/variable.test
tests/srfi-1.test
tests/srfi-9.test
tests/srfi-13.test
tests/srfi-14.test
tests/srfi-37.test
tests/srfi-39.test
tests/srfi-43.test
tests/optargs.test
tests/fluids.test
tests/catch.test
tests/getopt-long.test
tests/guile.test
tests/guile-module.test
tests/syntax.test
tests/let-syntax.test
tests/pmatch.test
tests/posix.test
tests/match.test
"

# Allow for make check MES_TESTS=tests/optargs.test
TESTS="${MES_TESTS-$mes_tests}"

xfail_tests="
tests/psyntax.test
"

# Allow for make check MES_XFAIL_TESTS=tests/optargs.test
XFAIL_TESTS="${MES_XFAIL_TESTS-$xfail_tests}"

test_ext=.test
log_compiler=${SHELL}
. ${srcdest}build-aux/test-suite.sh
