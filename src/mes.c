/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <libmes.h>

//#define MES_MINI 1
#if POSIX
long ARENA_SIZE = 100000000; // 2.3GiB
#else
long ARENA_SIZE = 300000; // 32b: 3MiB, 64b: 6 MiB
#endif
long MAX_ARENA_SIZE = 100000000;
long STACK_SIZE = 20000;

long JAM_SIZE = 20000;
long GC_SAFETY = 2000;

char *g_arena = 0;
typedef long SCM;

int g_debug = 0;
long g_free = 0;

SCM g_continuations = 0;
SCM g_symbols = 0;
SCM g_stack = 0;
SCM *g_stack_array = 0;
#define FRAME_SIZE 5
#define FRAME_PROCEDURE 4
// a/env
SCM r0 = 0;
// param 1
SCM r1 = 0;
// save 2+load/dump
SCM r2 = 0;
// continuation
SCM r3 = 0;
// current-module
SCM m0 = 0;
// macro
SCM g_macros = 0;
SCM g_ports = 1;

#if __x86_64__
#define HALFLONG_MAX UINT_MAX
typedef int halflong;
#else
#define HALFLONG_MAX UINT16_MAX
typedef short halflong;
#endif

#if __M2_PLANET__
CONSTANT TBYTES         0
CONSTANT TCHAR          1
CONSTANT TCLOSURE       2
CONSTANT TCONTINUATION  3
CONSTANT TFUNCTION      4
CONSTANT TKEYWORD       5
CONSTANT TMACRO         6
CONSTANT TNUMBER        7
CONSTANT TPAIR          8
CONSTANT TPORT          9
CONSTANT TREF          10
CONSTANT TSPECIAL      11
CONSTANT TSTRING       12
CONSTANT TSTRUCT       13
CONSTANT TSYMBOL       14
CONSTANT TVALUES       15
CONSTANT TVARIABLE     16
CONSTANT TVECTOR       17
CONSTANT TBROKEN_HEART 18
#else // !__M2_PLANET__
enum type_t {TBYTES, TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TPORT, TREF, TSPECIAL, TSTRING, TSTRUCT, TSYMBOL, TVALUES, TVARIABLE, TVECTOR, TBROKEN_HEART};
#endif // !__M2_PLANET__

typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);
#if !POSIX
struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};
struct function {
#if __M2_PLANET__
  FUNCTION function;
#else // !__M2_PLANET__
  SCM (*function) (SCM);
#endif // !__M2_PLANET__
  long arity;
  char *name;
};
#else
struct function {
  union {
    function0_t function0;
    function1_t function1;
    function2_t function2;
    function3_t function3;
    functionn_t functionn;
  };
  long arity;
  char const *name;
};
struct scm {
  enum type_t type;
  union
  {
#if 0
    struct
    {
      unsigned halflong start;
      unsigned halflong end;
    };
#endif
    unsigned long function;
    unsigned long length;
    long port;
    SCM car;
    SCM macro;
    SCM ref;
    SCM variable;
  };
  union
  {
    long value;
    char const* name;
    char const* bytes;
    SCM cdr;
    SCM closure;
    SCM continuation;
    SCM string;
    SCM vector;
  };
};
#endif

#if __MESC__
//FIXME
char *foobar = 0;
struct scm *g_cells = foobar;
struct scm *g_news = foobar;
#else
struct scm *g_cells = 0;
struct scm *g_news = 0;
#endif

struct scm scm_nil = {TSPECIAL, 0, "()"};
struct scm scm_f = {TSPECIAL, 0, "#f"};
struct scm scm_t = {TSPECIAL, 0, "#t"};
struct scm scm_dot = {TSPECIAL, 0, "."};
struct scm scm_arrow = {TSPECIAL, 0, "=>"};
struct scm scm_undefined = {TSPECIAL, 0, "*undefined*"};
struct scm scm_unspecified = {TSPECIAL, 0, "*unspecified*"};
struct scm scm_closure = {TSPECIAL, 0, "*closure*"};
struct scm scm_circular = {TSPECIAL, 0, "*circular*"};
struct scm scm_begin = {TSPECIAL, 0, "*begin*"};

struct scm scm_symbol_dot = {TSYMBOL, 0, "*dot*"};
struct scm scm_symbol_lambda = {TSYMBOL, 0, "lambda"};
struct scm scm_symbol_begin = {TSYMBOL, 0, "begin"};
struct scm scm_symbol_if = {TSYMBOL, 0, "if"};
struct scm scm_symbol_quote = {TSYMBOL, 0, "quote"};
struct scm scm_symbol_define = {TSYMBOL, 0, "define"};
struct scm scm_symbol_define_macro = {TSYMBOL, 0, "define-macro"};

struct scm scm_symbol_quasiquote = {TSYMBOL, 0, "quasiquote"};
struct scm scm_symbol_unquote = {TSYMBOL, 0, "unquote"};
struct scm scm_symbol_unquote_splicing = {TSYMBOL, 0, "unquote-splicing"};
struct scm scm_symbol_syntax = {TSYMBOL, 0, "syntax"};
struct scm scm_symbol_quasisyntax = {TSYMBOL, 0, "quasisyntax"};
struct scm scm_symbol_unsyntax = {TSYMBOL, 0, "unsyntax"};
struct scm scm_symbol_unsyntax_splicing = {TSYMBOL, 0, "unsyntax-splicing"};

struct scm scm_symbol_set_x = {TSYMBOL, 0, "set!"};

struct scm scm_symbol_sc_expand = {TSYMBOL, 0, "sc-expand"};
struct scm scm_symbol_macro_expand = {TSYMBOL, 0, "macro-expand"};
struct scm scm_symbol_portable_macro_expand = {TSYMBOL, 0, "portable-macro-expand"};
struct scm scm_symbol_sc_expander_alist = {TSYMBOL, 0, "*sc-expander-alist*"};

struct scm scm_symbol_call_with_values = {TSYMBOL, 0, "call-with-values"};
struct scm scm_call_with_current_continuation = {TSPECIAL, 0, "*call/cc*"};
struct scm scm_symbol_call_with_current_continuation = {TSYMBOL, 0, "call-with-current-continuation"};
struct scm scm_symbol_boot_module = {TSYMBOL, 0, "boot-module"};
struct scm scm_symbol_current_module = {TSYMBOL, 0, "current-module"};
struct scm scm_symbol_primitive_load = {TSYMBOL, 0, "primitive-load"};
struct scm scm_symbol_read_input_file = {TSYMBOL, 0, "read-input-file"};
struct scm scm_symbol_write = {TSYMBOL, 0, "write"};
struct scm scm_symbol_display = {TSYMBOL, 0, "display"};

struct scm scm_symbol_throw = {TSYMBOL, 0, "throw"};
struct scm scm_symbol_not_a_number = {TSYMBOL, 0, "not-a-number"};
struct scm scm_symbol_not_a_pair = {TSYMBOL, 0, "not-a-pair"};
struct scm scm_symbol_system_error = {TSYMBOL, 0, "system-error"};
struct scm scm_symbol_wrong_number_of_args = {TSYMBOL, 0, "wrong-number-of-args"};
struct scm scm_symbol_wrong_type_arg = {TSYMBOL, 0, "wrong-type-arg"};
struct scm scm_symbol_unbound_variable = {TSYMBOL, 0, "unbound-variable"};

struct scm scm_symbol_hashq_table = {TSYMBOL, 0, "<hashq-table>"};
struct scm scm_symbol_record_type = {TSYMBOL, 0, "<record-type>"};
struct scm scm_symbol_frame = {TSYMBOL, 0, "<frame>"};
struct scm scm_symbol_module = {TSYMBOL, 0, "<module>"};
struct scm scm_symbol_stack = {TSYMBOL, 0, "<stack>"};
struct scm scm_symbol_buckets = {TSYMBOL, 0, "buckets"};
struct scm scm_symbol_procedure = {TSYMBOL, 0, "procedure"};
struct scm scm_symbol_size = {TSYMBOL, 0, "size"};

struct scm scm_symbol_argv = {TSYMBOL, 0, "%argv"};
struct scm scm_symbol_mes_prefix = {TSYMBOL, 0, "%prefix"};
struct scm scm_symbol_mes_version = {TSYMBOL, 0, "%version"};

struct scm scm_symbol_car = {TSYMBOL, 0, "car"};
struct scm scm_symbol_cdr = {TSYMBOL, 0, "cdr"};
struct scm scm_symbol_pmatch_car = {TSYMBOL, 0, "pmatch-car"};
struct scm scm_symbol_pmatch_cdr = {TSYMBOL, 0, "pmatch-cdr"};

struct scm scm_vm_evlis = {TSPECIAL, 0, "*vm-evlis*"};
struct scm scm_vm_evlis2 = {TSPECIAL, 0, "*vm-evlis2*"};
struct scm scm_vm_evlis3 = {TSPECIAL, 0, "*vm-evlis3*"};
struct scm scm_vm_apply = {TSPECIAL, 0, "core:apply"};
struct scm scm_vm_apply2 = {TSPECIAL, 0, "*vm-apply2*"};
struct scm scm_vm_eval = {TSPECIAL, 0, "core:eval-expanded"};

struct scm scm_vm_eval_pmatch_car = {TSPECIAL, 0, "*vm-eval-pmatch-car*"};
struct scm scm_vm_eval_pmatch_cdr = {TSPECIAL, 0, "*vm-eval-pmatch-cdr*"};
struct scm scm_vm_eval_define = {TSPECIAL, 0, "*vm-eval-define*"};

struct scm scm_vm_eval_set_x = {TSPECIAL, 0, "*vm-eval-set!*"};
struct scm scm_vm_eval_macro_expand_eval = {TSPECIAL, 0, "*vm:eval-macro-expand-eval*"};
struct scm scm_vm_eval_macro_expand_expand = {TSPECIAL, 0, "*vm:eval-macro-expand-expand*"};
struct scm scm_vm_eval_check_func = {TSPECIAL, 0, "*vm-eval-check-func*"};
struct scm scm_vm_eval2 = {TSPECIAL, 0, "*vm-eval2*"};
struct scm scm_vm_macro_expand = {TSPECIAL, 0, "core:macro-expand"};
struct scm scm_vm_macro_expand_define = {TSPECIAL, 0, "*vm:core:macro-expand-define*"};
struct scm scm_vm_macro_expand_define_macro = {TSPECIAL, 0, "*vm:core:macro-expand-define-macro*"};
struct scm scm_vm_macro_expand_lambda = {TSPECIAL, 0, "*vm:core:macro-expand-lambda*"};
struct scm scm_vm_macro_expand_set_x = {TSPECIAL, 0, "*vm:core:macro-expand-set!*"};
struct scm scm_vm_begin_expand_primitive_load = {TSPECIAL, 0, "*vm:core:begin-expand-primitive-load*"};
struct scm scm_vm_begin_primitive_load = {TSPECIAL, 0, "*vm:core:begin-primitive-load*"};
struct scm scm_vm_macro_expand_car = {TSPECIAL, 0, "*vm:core:macro-expand-car*"};
struct scm scm_vm_macro_expand_cdr = {TSPECIAL, 0, "*vm:macro-expand-cdr*"};
struct scm scm_vm_begin_expand = {TSPECIAL, 0, "core:eval"};
struct scm scm_vm_begin_expand_eval = {TSPECIAL, 0, "*vm:begin-expand-eval*"};
struct scm scm_vm_begin_expand_macro = {TSPECIAL, 0, "*vm:begin-expand-macro*"};
struct scm scm_vm_begin = {TSPECIAL, 0, "*vm-begin*"};
struct scm scm_vm_begin_read_input_file = {TSPECIAL, 0, "*vm-begin-read-input-file*"};
struct scm scm_vm_begin_eval = {TSPECIAL, 0, "*vm:begin-eval*"};
struct scm scm_vm_if = {TSPECIAL, 0, "*vm-if*"};
struct scm scm_vm_if_expr = {TSPECIAL, 0, "*vm-if-expr*"};
struct scm scm_vm_call_with_values2 = {TSPECIAL, 0, "*vm-call-with-values2*"};
struct scm scm_vm_call_with_current_continuation2 = {TSPECIAL, 0, "*vm-call-with-current-continuation2*"};
struct scm scm_vm_return = {TSPECIAL, 0, "*vm-return*"};

struct scm scm_type_bytes = {TSYMBOL, 0, "<cell:bytes>"};
struct scm scm_type_char = {TSYMBOL, 0, "<cell:char>"};
struct scm scm_type_closure = {TSYMBOL, 0, "<cell:closure>"};
struct scm scm_type_continuation = {TSYMBOL, 0, "<cell:continuation>"};
struct scm scm_type_function = {TSYMBOL, 0, "<cell:function>"};
struct scm scm_type_keyword = {TSYMBOL, 0, "<cell:keyword>"};
struct scm scm_type_macro = {TSYMBOL, 0, "<cell:macro>"};
struct scm scm_type_number = {TSYMBOL, 0, "<cell:number>"};
struct scm scm_type_pair = {TSYMBOL, 0, "<cell:pair>"};
struct scm scm_type_port = {TSYMBOL, 0, "<cell:port>"};
struct scm scm_type_ref = {TSYMBOL, 0, "<cell:ref>"};
struct scm scm_type_special = {TSYMBOL, 0, "<cell:special>"};
struct scm scm_type_string = {TSYMBOL, 0, "<cell:string>"};
struct scm scm_type_struct = {TSYMBOL, 0, "<cell:struct>"};
struct scm scm_type_symbol = {TSYMBOL, 0, "<cell:symbol>"};
struct scm scm_type_values = {TSYMBOL, 0, "<cell:values>"};
struct scm scm_type_variable = {TSYMBOL, 0, "<cell:variable>"};
struct scm scm_type_vector = {TSYMBOL, 0, "<cell:vector>"};
struct scm scm_type_broken_heart = {TSYMBOL, 0, "<cell:broken-heart>"};

struct scm scm_symbol_internal_time_units_per_second = {TSYMBOL, 0, "internal-time-units-per-second"};
struct scm scm_symbol_compiler = {TSYMBOL, 0, "%compiler"};
struct scm scm_symbol_arch = {TSYMBOL, 0, "%arch"};

struct scm scm_test = {TSYMBOL, 0, "%%test"};

#if !POSIX
#include "src/mes.mes.symbols.h"
#else
#include "src/mes.symbols.h"
#endif

struct function g_functions[200];
int g_function = 0;

#if !__GNUC__ || !POSIX
#include "src/gc.mes.h"
#include "src/hash.mes.h"
#include "src/lib.mes.h"
#include "src/math.mes.h"
#include "src/mes.mes.h"
#include "src/module.mes.h"
#include "src/posix.mes.h"
#include "src/reader.mes.h"
#include "src/strings.mes.h"
#include "src/struct.mes.h"
#include "src/vector.mes.h"
#else
#include "src/gc.h"
#include "src/hash.h"
#include "src/lib.h"
#include "src/math.h"
#include "src/mes.h"
#include "src/module.h"
#include "src/posix.h"
#include "src/reader.h"
#include "src/strings.h"
#include "src/struct.h"
#include "src/vector.h"
#endif

#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr

#define NTYPE(x) g_news[x].type
#define NCAR(x) g_news[x].car
#define NCDR(x) g_news[x].cdr

#if !POSIX
#define BYTES(x) g_cells[x].car
#define LENGTH(x) g_cells[x].car
#define REF(x) g_cells[x].car
#define START(x) (g_cells[x].car >> 16)
#define LEN(x) (g_cells[x].car & 0xffff)
#define VARIABLE(x) g_cells[x].car

#define CLOSURE(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr

#define CBYTES(x) &g_cells[x].cdr
#define CSTRING_STRUCT(x) &g_cells[x.cdr].cdr

#define FUNCTION(x) g_functions[g_cells[x].car]
#define FUNCTION0(x) g_functions[g_cells[x].car].function
#define MACRO(x) g_cells[x].car
#define NAME(x) g_cells[x].cdr
#define PORT(x) g_cells[x].car
#define STRING(x) g_cells[x].cdr
#define STRUCT(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr

#define NLENGTH(x) g_news[x].car
#define NCBYTES(x) &g_news[x].cdr
#define NVALUE(x) g_news[x].cdr
#define NSTRING(x) g_news[x].cdr
#define NVECTOR(x) g_news[x].cdr

#else
#define BYTES(x) g_cells[x].bytes
#define FUNCTION(x) g_functions[g_cells[x].function]
#define FUNCTION0(x) g_functions[g_cells[x].function].function0
#define LENGTH(x) g_cells[x].length
#define REF(x) g_cells[x].ref
#define START(x) g_cells[x].start
#define LEN(x) g_cells[x].end
#define VARIABLE(x) g_cells[x].variable

#define CLOSURE(x) g_cells[x].closure
#define CBYTES(x) &g_cells[x].bytes
#define CSTRING_STRUCT(x) &g_cells[x.string].string
#define CONTINUATION(x) g_cells[x].continuation
#define MACRO(x) g_cells[x].macro
#define NAME(x) g_cells[x].name
#define PORT(x) g_cells[x].port
#define STRING(x) g_cells[x].string
#define STRUCT(x) g_cells[x].vector
#define VALUE(x) g_cells[x].value
#define VECTOR(x) g_cells[x].vector

#define NLENGTH(x) g_news[x].length

#define NCBYTES(x) &g_news[x].bytes
#define NVALUE(x) g_news[x].value
#define NVECTOR(x) g_news[x].vector
#endif

#define CSTRING(x) CBYTES (STRING (x))

#define MAKE_BYTES0(x) make_bytes (x, strlen (x))
#define NAME_SYMBOL(symbol,name) {size_t s = strlen (name); CAR (symbol) = s; CDR (symbol) = make_bytes (name, s);}

#define MAKE_CHAR(n) make_cell__ (TCHAR, 0, n)
#define MAKE_CONTINUATION(n) make_cell__ (TCONTINUATION, n, g_stack)
#define MAKE_NUMBER(n) make_cell__ (TNUMBER, 0, n)
#define MAKE_REF(n) make_cell__ (TREF, n, 0)
#define MAKE_STRING0(x) make_string (x, strlen (x))
#define MAKE_STRING_PORT(x) make_cell__ (TPORT, -length__ (g_ports) - 2, x)
#define MAKE_MACRO(name, x) make_cell__ (TMACRO, x, STRING (name))

#define CAAR(x) CAR (CAR (x))
#define CADR(x) CAR (CDR (x))
#define CDAR(x) CDR (CAR (x))
#define CDDR(x) CDR (CDR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))

SCM make_bytes (char const* s, size_t length);
SCM cstring_to_list (char const* s);
SCM string_equal_p (SCM a, SCM b);

SCM
alloc (long n)
{
  SCM x = g_free;
  g_free += n;
  if (g_free > ARENA_SIZE)
    assert (!"alloc: out of memory");
  return x;
}

SCM
make_cell__ (long type, SCM car, SCM cdr)
{
  SCM x = alloc (1);
  TYPE (x) = type;
  CAR (x) = car;
  CDR (x) = cdr;
  return x;
}

SCM
make_cell_ (SCM type, SCM car, SCM cdr)
{
  assert (TYPE (type) == TNUMBER);
  long t = VALUE (type);
  if (t == TCHAR || t == TNUMBER)
    return make_cell__ (t, car ? CAR (car) : 0, cdr ? CDR (cdr) : 0);
  return make_cell__ (t, car, cdr);
}

SCM
assoc_string (SCM x, SCM a) ///((internal))
{
  while (a != cell_nil && (TYPE (CAAR (a)) != TSTRING
                           || string_equal_p (x, CAAR (a)) == cell_f))
    a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
type_ (SCM x)
{
  return MAKE_NUMBER (TYPE (x));
}

// SCM
// car_to_cell_ (SCM x)
// {
//   return CAR (x);
// }

// SCM
// cdr_to_cell_ (SCM x)
// {
//   return CDR (x);
// }

// SCM
// car_to_number_ (SCM x)
// {
//   return MAKE_NUMBER (CAR (x));
// }

// SCM
// cdr_to_number_ (SCM x)
// {
//   return MAKE_NUMBER (CDR (x));
// }

SCM
car_ (SCM x)
{
  return (TYPE (x) != TCONTINUATION
          && (TYPE (CAR (x)) == TPAIR // FIXME: this is weird
              || TYPE (CAR (x)) == TREF
              || TYPE (CAR (x)) == TSPECIAL
              || TYPE (CAR (x)) == TSYMBOL
              || TYPE (CAR (x)) == TSTRING)) ? CAR (x) : MAKE_NUMBER (CAR (x));
}

SCM
cdr_ (SCM x)
{
  return (TYPE (x) != TCHAR
          && TYPE (x) != TNUMBER
          && TYPE (x) != TPORT
          && (TYPE (CDR (x)) == TPAIR
              || TYPE (CDR (x)) == TREF
              || TYPE (CDR (x)) == TSPECIAL
              || TYPE (CDR (x)) == TSYMBOL
              || TYPE (CDR (x)) == TSTRING)) ? CDR (x) : MAKE_NUMBER (CDR (x));
}

SCM
arity_ (SCM x)
{
  assert (TYPE (x) == TFUNCTION);
  return MAKE_NUMBER (FUNCTION (x).arity);
}

SCM
cons (SCM x, SCM y)
{
  return make_cell__ (TPAIR, x, y);
}

SCM
car (SCM x)
{
#if !__MESC_MES__
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
#endif
  return CAR (x);
}

SCM
cdr (SCM x)
{
#if !__MESC_MES__
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
#endif
  return CDR (x);
}

SCM
list (SCM x) ///((arity . n))
{
  return x;
}

SCM
null_p (SCM x)
{
  return x == cell_nil ? cell_t : cell_f;
}

SCM
eq_p (SCM x, SCM y)
{
  return (x == y
          || ((TYPE (x) == TKEYWORD && TYPE (y) == TKEYWORD
               && string_equal_p (x, y) == cell_t))
          || (TYPE (x) == TCHAR && TYPE (y) == TCHAR
              && VALUE (x) == VALUE (y))
          || (TYPE (x) == TNUMBER && TYPE (y) == TNUMBER
              && VALUE (x) == VALUE (y)))
    ? cell_t : cell_f;
}

SCM
values (SCM x) ///((arity . n))
{
  SCM v = cons (0, x);
  TYPE (v) = TVALUES;
  return v;
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

long
length__ (SCM x) ///((internal))
{
  long n = 0;
  while (x != cell_nil)
    {
      n++;
      if (TYPE (x) != TPAIR)
        return -1;
      x = CDR (x);
    }
  return n;
}

SCM
length (SCM x)
{
  return MAKE_NUMBER (length__ (x));
}

SCM apply (SCM, SCM, SCM);

SCM
error (SCM key, SCM x)
{
#if !__MESC_MES__
  SCM throw;
  if ((throw = module_ref (r0, cell_symbol_throw)) != cell_undefined)
    return apply (throw, cons (key, cons (x, cell_nil)), r0);
#endif
  display_error_ (key);
  eputs (": ");
  write_error_ (x);
  eputs ("\n");
  assert (0);
  exit (1);
}

//  extra lib
SCM
assert_defined (SCM x, SCM e) ///((internal))
{
  if (e == cell_undefined)
    return error (cell_symbol_unbound_variable, x);
  return e;
}

SCM make_string (char const* s, size_t length);

SCM
check_formals (SCM f, SCM formals, SCM args) ///((internal))
{
  long flen = (TYPE (formals) == TNUMBER) ? VALUE (formals) : length__ (formals);
  long alen = length__ (args);
  if (alen != flen && alen != -1 && flen != -1)
    {
      char *s = "apply: wrong number of arguments; expected: ";
      eputs (s);
      eputs (itoa (flen));
      eputs (", got: ");
      eputs (itoa (alen));
      eputs ("\n");
      write_error_ (f);
      SCM e = MAKE_STRING0 (s);
      return error (cell_symbol_wrong_number_of_args, cons (e, f));
    }
  return cell_unspecified;
}

SCM
check_apply (SCM f, SCM e) ///((internal))
{
  char* type = 0;
  if (f == cell_f || f == cell_t)
    type = "bool";
  if (f == cell_nil)
    type = "nil";
  if (f == cell_unspecified)
    type = "*unspecified*";
  if (f == cell_undefined)
    type = "*undefined*";
  if (TYPE (f) == TCHAR)
    type = "char";
  if (TYPE (f) == TNUMBER)
    type = "number";
  if (TYPE (f) == TSTRING)
    type = "string";
  if (TYPE (f) == TSTRUCT)
    type = "#<...>";
  if (TYPE (f) == TBROKEN_HEART)
    type = "<3";

  if (type)
    {
      char *s = "cannot apply: ";
      eputs (s);
      eputs (type);
      eputs ("[");
      write_error_ (e);
      eputs ("]\n");
      SCM e = MAKE_STRING0 (s);
      return error (cell_symbol_wrong_type_arg, cons (e, f));
    }
  return cell_unspecified;
}

SCM
gc_push_frame () ///((internal))
{
  if (g_stack < 5)
    assert (!"STACK FULL");
  g_stack_array[--g_stack] = cell_f;
  g_stack_array[--g_stack] = r0;
  g_stack_array[--g_stack] = r1;
  g_stack_array[--g_stack] = r2;
  g_stack_array[--g_stack] = r3;
  return g_stack;
}

SCM
gc_peek_frame () ///((internal))
{
  r3 = g_stack_array[g_stack];
  r2 = g_stack_array[g_stack+1];
  r1 = g_stack_array[g_stack+2];
  r0 = g_stack_array[g_stack+3];
  return g_stack_array[g_stack+FRAME_PROCEDURE];
}

SCM
gc_pop_frame () ///((internal))
{
  SCM x = gc_peek_frame ();
  g_stack += 5;
  return x;
}

SCM
append2 (SCM x, SCM y)
{
  if (x == cell_nil)
    return y;
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_append2));
  SCM r = cell_nil;
  while (x != cell_nil)
    {
      r = cons (CAR (x), r);
      x = CDR (x);
    }
  return reverse_x_ (r, y);
}

SCM
append_reverse (SCM x, SCM y)
{
  if (x == cell_nil)
    return y;
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_append_reverse));
  while (x != cell_nil)
    {
      y = cons (CAR (x), y);
      x = CDR (x);
    }
  return y;
}

SCM
reverse_x_ (SCM x, SCM t)
{
  if (x != cell_nil && TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_reverse_x_));
  SCM r = t;
  while (x != cell_nil)
    {
      t = CDR (x);
      CDR (x) = r;
      r = x;
      x = t;
    }
  return r;
}

SCM
pairlis (SCM x, SCM y, SCM a)
{
  if (x == cell_nil)
    return a;
  if (TYPE (x) != TPAIR)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}

SCM
call (SCM fn, SCM x)
{
#if __M2_PLANET__
  struct function *f = FUNCTION (fn);
#else
  struct function *f = &FUNCTION (fn);
#endif
  int arity = f->arity;
  if ((arity > 0 || arity == -1)
      && x != cell_nil && TYPE (CAR (x)) == TVALUES)
    x = cons (CADAR (x), CDR (x));
  if ((arity > 1 || arity == -1)
      && x != cell_nil && TYPE (CDR (x)) == TPAIR && TYPE (CADR (x)) == TVALUES)
    x = cons (CAR (x), cons (CDADAR (x), CDR (x)));

#if __M2_PLANET__
  FUNCTION fp = f->function;
  if (arity == 0)
    return fp ();
  else if (arity == 1)
    return fp (CAR (x));
  else if (arity == 2)
    return fp (CAR (x), CADR (x));
  else if (arity == 3)
    return fp (CAR (x), CADR (x), CAR (CDDR (x)));
  else if (arity == -1)
    return fp (x);
#elif !POSIX
  if (arity == 0)
    {
      //function0_t fp = f->function;
      SCM (*fp) (void) = f->function;
      return fp ();
    }
  else if (arity == 1)
    {
      //function1_t fp = f->function;
      SCM (*fp) (SCM) = f->function;
      return fp (CAR (x));
    }
  else if (arity == 2)
    {
      //function2_t fp = f->function;
      SCM (*fp) (SCM, SCM) = f->function;
      return fp (CAR (x), CADR (x));
    }
  else if (arity == 3)
    {
      //function3_t fp = f->function;
      SCM (*fp) (SCM, SCM, SCM) = f->function;
      return fp (CAR (x), CADR (x), CAR (CDDR (x)));
    }
  else if (arity == -1)
    {
      //functionn_t fp = f->function;
      SCM (*fp) (SCM) = f->function;
      return fp (x);
    }
#else
  if (arity == 0)
    return FUNCTION (fn).function0 ();
  else if (arity == 1)
    return FUNCTION (fn).function1 (CAR (x));
  else if (arity == 2)
    return FUNCTION (fn).function2 (CAR (x), CADR (x));
  else if (arity == 3)
    return FUNCTION (fn).function3 (CAR (x), CADR (x), CAR (CDDR (x)));
  else if (arity == -1)
    return FUNCTION (fn).functionn (x);
#endif //! (__M2_PLANET__ || !POSIX)
  return cell_unspecified;
}

SCM
assq (SCM x, SCM a)
{
  if (TYPE (a) != TPAIR)
    return cell_f;
  int t = TYPE (x);
  if (t == TSYMBOL
      || t == TSPECIAL)
    while (a != cell_nil && x != CAAR (a))
      a = CDR (a);
  else if (t == TCHAR
           || t == TNUMBER)
      {
        SCM v = VALUE (x);
        while (a != cell_nil && v != VALUE (CAAR (a)))
          a = CDR (a);
      }
  else if (t == TKEYWORD)
    {
      while (a != cell_nil && string_equal_p (x, CAAR (a)) == cell_f)
        a = CDR (a);
    }
  else
    /* pointer equality, e.g. on strings. */
    while (a != cell_nil && x != CAAR (a))
      a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
assoc (SCM x, SCM a)
{
  if (TYPE (x) == TSTRING)
    return assoc_string (x, a);
  while (a != cell_nil && equal2_p (x, CAAR (a)) == cell_f)
    a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
set_car_x (SCM x, SCM e)
{
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_set_car_x));
  CAR (x) = e;
  return cell_unspecified;
}

SCM
set_cdr_x (SCM x, SCM e)
{
  if (TYPE (x) != TPAIR)
    error (cell_symbol_not_a_pair, cons (x, cell_set_cdr_x));
  CDR (x) = e;
  return cell_unspecified;
}

SCM
set_env_x (SCM x, SCM e, SCM a)
{
  SCM p;
  if (TYPE (x) == TVARIABLE)
    p = VARIABLE (x);
  else
    p = assert_defined (x, module_variable (a, x));
  if (TYPE (p) != TPAIR)
    error (cell_symbol_not_a_pair, cons (p, x));
  return set_cdr_x (p, e);
}

SCM
call_lambda (SCM e, SCM x, SCM aa, SCM a) ///((internal))
{
  SCM cl = cons (cons (cell_closure, x), x);
  r1 = e;
  r0 = cl;
  return cell_unspecified;
}

SCM
make_closure_ (SCM args, SCM body, SCM a) ///((internal))
{
  return make_cell__ (TCLOSURE, cell_f, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
make_variable_ (SCM var) ///((internal))
{
  return make_cell__ (TVARIABLE, var, 0);
}

SCM
macro_get_handle (SCM name)
{
  if (TYPE (name) == TSYMBOL)
    return hashq_get_handle (g_macros, name, cell_nil);
  return cell_f;
}

SCM
get_macro (SCM name) ///((internal))
{
  SCM m = macro_get_handle (name);
  if (m != cell_f)
    return MACRO (CDR (m));
  return cell_f;
}

SCM
macro_set_x (SCM name, SCM value) ///((internal))
{
  return hashq_set_x (g_macros, name, value);
}

SCM
push_cc (SCM p1, SCM p2, SCM a, SCM c) ///((internal))
{
  SCM x = r3;
  r3 = c;
  r2 = p2;
  gc_push_frame ();
  r1 = p1;
  r0 = a;
  r3 = x;
  return cell_unspecified;
}

SCM
add_formals (SCM formals, SCM x)
{
  while (TYPE (x) == TPAIR)
    {
      formals = cons (CAR (x), formals);
      x = CDR (x);
    }
  if (TYPE (x) == TSYMBOL)
    formals = cons (x, formals);
  return formals;
}

int
formal_p (SCM x, SCM formals) /// ((internal))
{
  if (TYPE (formals) == TSYMBOL)
    {
      if (x == formals)
        return x;
      else return cell_f;
    }
  while (TYPE (formals) == TPAIR && CAR (formals) != x)
    formals = CDR (formals);
  if (TYPE (formals) == TSYMBOL)
    return formals == x;
  return TYPE (formals) == TPAIR;
}

SCM
expand_variable_ (SCM x, SCM formals, int top_p) ///((internal))
{
  while (TYPE (x) == TPAIR)
    {
      if (TYPE (CAR (x)) == TPAIR)
        {
          if (CAAR (x) == cell_symbol_lambda)
            {
              SCM f = CAR (CDAR (x));
              formals = add_formals (formals, f);
            }
          else if (CAAR (x) == cell_symbol_define
                   || CAAR (x) == cell_symbol_define_macro)
            {
              SCM f = CAR (CDAR (x));
              formals = add_formals (formals, f);
            }
          if (CAAR (x) != cell_symbol_quote)
            expand_variable_ (CAR (x), formals, 0);
        }
      else
        {
          if (CAR (x) == cell_symbol_lambda)
            {
              SCM f = CADR (x);
              formals = add_formals (formals, f);
              x = CDR (x);
            }
          else if (CAR (x) == cell_symbol_define
                   || CAR (x) == cell_symbol_define_macro)
            {
              SCM f = CADR (x);
              if (top_p && TYPE (f) == TPAIR)
                f = CDR (f);
              formals = add_formals (formals, f);
              x = CDR (x);
            }
          else if (CAR (x) == cell_symbol_quote)
            return cell_unspecified;
          else if (TYPE (CAR (x)) == TSYMBOL
                   && CAR (x) != cell_symbol_boot_module
                   && CAR (x) != cell_symbol_current_module
                   && CAR (x) != cell_symbol_primitive_load
                   && !formal_p (CAR (x), formals))
            {
              SCM v = module_variable (r0, CAR (x));
              if (v != cell_f)
                CAR (x) = make_variable_ (v);
            }
        }
      x = CDR (x);
      top_p = 0;
    }
  return cell_unspecified;
}

SCM
expand_variable (SCM x, SCM formals) ///((internal))
{
  return expand_variable_ (x, formals, 1);
}

SCM struct_ref_ (SCM x, long i);
SCM vector_ref_ (SCM x, long i);
SCM make_vector__ (long k);
SCM vector_set_x_ (SCM x, long i, SCM e);

SCM
eval_apply ()
{
  SCM aa;
  SCM args;
  SCM body;
  SCM cl;
  SCM entry;
  SCM expanders;
  SCM formals;
  SCM input;
  SCM name;
  SCM macro;
  SCM p;
  SCM program;
  SCM sc_expand;
  SCM v;
  SCM x;
  int global_p;
  int macro_p;
  int t;
  long c;

 eval_apply:
  if (r3 == cell_vm_evlis2) goto evlis2;
  else if (r3 == cell_vm_evlis3) goto evlis3;
  else if (r3 == cell_vm_eval_check_func) goto eval_check_func;
  else if (r3 == cell_vm_eval2) goto eval2;
  else if (r3 == cell_vm_apply2) goto apply2;
  else if (r3 == cell_vm_if_expr) goto if_expr;
  else if (r3 == cell_vm_begin_eval) goto begin_eval;
  else if (r3 == cell_vm_eval_set_x) goto eval_set_x;
  else if (r3 == cell_vm_macro_expand_car) goto macro_expand_car;
  else if (r3 == cell_vm_return) goto vm_return;
  else if (r3 == cell_vm_macro_expand_cdr) goto macro_expand_cdr;
  else if (r3 == cell_vm_eval_define) goto eval_define;
  else if (r3 == cell_vm_macro_expand) goto macro_expand;
  else if (r3 == cell_vm_macro_expand_lambda) goto macro_expand_lambda;
  else if (r3 == cell_vm_eval_pmatch_car) goto eval_pmatch_car;
  else if (r3 == cell_vm_begin_expand_macro) goto begin_expand_macro;
  else if (r3 == cell_vm_macro_expand_define) goto macro_expand_define;
  else if (r3 == cell_vm_begin_expand_eval) goto begin_expand_eval;
  else if (r3 == cell_vm_call_with_current_continuation2) goto call_with_current_continuation2;
  else if (r3 == cell_vm_macro_expand_set_x) goto macro_expand_set_x;
  else if (r3 == cell_vm_eval_pmatch_cdr) goto eval_pmatch_cdr;
  else if (r3 == cell_vm_macro_expand_define_macro) goto macro_expand_define_macro;
  else if (r3 == cell_vm_begin_primitive_load) goto begin_primitive_load;

  else if (r3 == cell_vm_evlis) goto evlis;
  else if (r3 == cell_vm_apply) goto apply;
  else if (r3 == cell_vm_eval) goto eval;
  else if (r3 == cell_vm_eval_macro_expand_eval) goto eval_macro_expand_eval;
  else if (r3 == cell_vm_eval_macro_expand_expand) goto eval_macro_expand_expand;
  else if (r3 == cell_vm_begin) goto begin;
  else if (r3 == cell_vm_begin_expand) goto begin_expand;
  else if (r3 == cell_vm_begin_expand_primitive_load) goto begin_expand_primitive_load;
  else if (r3 == cell_vm_if) goto vm_if;
  else if (r3 == cell_vm_call_with_values2) goto call_with_values2;
  else if (r3 == cell_unspecified) return r1;
  else
    error (cell_symbol_system_error,
           MAKE_STRING0 ("eval/apply unknown continuation"));

 evlis:
  if (r1 == cell_nil)
    goto vm_return;
  if (TYPE (r1) != TPAIR)
    goto eval;
  push_cc (CAR (r1), r1, r0, cell_vm_evlis2);
  goto eval;
 evlis2:
  push_cc (CDR (r2), r1, r0, cell_vm_evlis3);
  goto evlis;
 evlis3:
  r1 = cons (r2, r1);
  goto vm_return;

 apply:
  g_stack_array[g_stack+FRAME_PROCEDURE] = CAR (r1);
  t = TYPE (CAR (r1));
  if (t == TFUNCTION)
    {
      check_formals (CAR (r1), MAKE_NUMBER (FUNCTION (CAR (r1)).arity), CDR (r1));
      r1 = call (CAR (r1), CDR (r1)); /// FIXME: move into eval_apply
      goto vm_return;
    }
  else if (t == TCLOSURE)
    {
      cl = CLOSURE (CAR (r1));
      body = CDDR (cl);
      formals = CADR (cl);
      args = CDR (r1);
      aa = CDAR (cl);
      aa = CDR (aa);
      check_formals (CAR (r1), formals, CDR (r1));
      p = pairlis (formals, args, aa);
      call_lambda (body, p, aa, r0);
      goto begin;
    }
  else if (t == TCONTINUATION)
    {
      v = CONTINUATION (CAR (r1));
      if (LENGTH (v))
        {
          for (t=0; t < LENGTH (v); t++)
            g_stack_array[STACK_SIZE-LENGTH (v)+t] = vector_ref_ (v, t);
          g_stack = STACK_SIZE-LENGTH (v);
        }
      x = r1;
      gc_pop_frame ();
      r1 = CADR (x);
      goto eval_apply;
    }
  else if (t == TSPECIAL)
    {
      c = CAR (r1);
      if (c == cell_vm_apply)
        {
          push_cc (cons (CADR (r1), CADDR (r1)), r1, r0, cell_vm_return);
          goto apply;
        }
      else if (c ==  cell_vm_eval)
        {
          push_cc (CADR (r1), r1, CADDR (r1), cell_vm_return);
          goto eval;
        }
      else if (c ==  cell_vm_begin_expand)
        {
          push_cc (cons (CADR (r1), cell_nil), r1, CADDR (r1), cell_vm_return);
          goto begin_expand;
        }
      else if (c ==  cell_call_with_current_continuation)
        {
          r1 = CDR (r1);
          goto call_with_current_continuation;
        }
      else
        check_apply (cell_f, CAR (r1));
    }
  else if (t == TSYMBOL)
    {
      if (CAR (r1) == cell_symbol_call_with_values)
        {
          r1 = CDR (r1);
          goto call_with_values;
        }
      if (CAR (r1) == cell_symbol_current_module)
        {
          r1 = r0;
          goto vm_return;
        }
      if (CAR (r1) == cell_symbol_boot_module)
        {
          r1 = m0;
          goto vm_return;
        }
    }
  else if (t == TPAIR)
    {
      if (CAAR (r1) == cell_symbol_lambda)
        {
          formals = CADR (CAR (r1));
          args = CDR (r1);
          body = CDDR (CAR (r1));
          p = pairlis (formals, CDR (r1), r0);
          check_formals (r1, formals, args);
          call_lambda (body, p, p, r0);
          goto begin;
        }
    }
  push_cc (CAR (r1), r1, r0, cell_vm_apply2);
  goto eval;
 apply2:
  check_apply (r1, CAR (r2));
  r1 = cons (r1, CDR (r2));
  goto apply;

 eval:
  t = TYPE (r1);
  if (t == TPAIR)
    {
      c = CAR (r1);
      if (c ==  cell_symbol_pmatch_car)
        {
          push_cc (CADR (r1), r1, r0, cell_vm_eval_pmatch_car);
          goto eval;
        eval_pmatch_car:
          x = r1;
          gc_pop_frame ();
          r1 = CAR (x);
          goto eval_apply;
        }
      else if (c ==  cell_symbol_pmatch_cdr)
        {
          push_cc (CADR (r1), r1, r0, cell_vm_eval_pmatch_cdr);
          goto eval;
        eval_pmatch_cdr:
          x = r1;
          gc_pop_frame ();
          r1 = CDR (x);
          goto eval_apply;
        }
      else if (c ==  cell_symbol_quote)
        {
          x = r1;
          gc_pop_frame ();
          r1 = CADR (x);
          goto eval_apply;
        }
      else if (c ==  cell_symbol_begin)
        goto begin;
      else if (c ==  cell_symbol_lambda)
        {
          r1 = make_closure_ (CADR (r1), CDDR (r1), r0);
          goto vm_return;
        }
      else if (c ==  cell_symbol_if)
        {
          r1=CDR (r1);
          goto vm_if;
        }
      else if (c ==  cell_symbol_set_x)
        {
          push_cc (CAR (CDDR (r1)), r1, r0, cell_vm_eval_set_x);
          goto eval;
        eval_set_x:
          r1 = set_env_x (CADR (r2), r1, r0);
          goto vm_return;
        }
      else if (c == cell_vm_macro_expand)
        {
          push_cc (CADR (r1), r1, r0, cell_vm_eval_macro_expand_eval);
          goto eval;
        eval_macro_expand_eval:
          push_cc (r1, r2, r0, cell_vm_eval_macro_expand_expand);
          goto macro_expand;
        eval_macro_expand_expand:
          goto vm_return;
        }
      else
        {
          if (TYPE (r1) == TPAIR
              && (CAR (r1) == cell_symbol_define
                  || CAR (r1) == cell_symbol_define_macro))
            {
              global_p = CAAR (r0) != cell_closure;
              macro_p = CAR (r1) == cell_symbol_define_macro;
              if (global_p)
                {
                  name = CADR (r1);
                  if (TYPE (CADR (r1)) == TPAIR)
                    name = CAR (name);
                  if (macro_p)
                    {
                      entry = assq (name, g_macros);
                      if (entry == cell_f)
                        macro_set_x (name, cell_f);
                    }
                  else
                    {
                      entry = module_variable (r0, name);
                      if (entry == cell_f)
                        module_define_x (m0, name, cell_f);
                    }
                }
              r2 = r1;
              if (TYPE (CADR (r1)) != TPAIR)
                {
                  push_cc (CAR (CDDR (r1)), r2, cons (cons (CADR (r1), CADR (r1)), r0), cell_vm_eval_define);
                  goto eval;
                }
              else
                {
                  p = pairlis (CADR (r1), CADR (r1), r0);
                  formals = CDR (CADR (r1));
                  body = CDDR (r1);

                  if (macro_p || global_p)
                    expand_variable (body, formals);
                  r1 = cons (cell_symbol_lambda, cons (formals, body));
                  push_cc (r1, r2, p, cell_vm_eval_define);
                  goto eval;
                }
            eval_define:;
              name = CADR (r2);
              if (TYPE (CADR (r2)) == TPAIR)
                name = CAR (name);
              if (macro_p)
                {
                  entry = macro_get_handle (name);
                  r1 = MAKE_MACRO (name, r1);
                  set_cdr_x (entry, r1);
                }
              else if (global_p)
                {
                  entry = module_variable (r0, name);
                  set_cdr_x (entry, r1);
                }
              else
                {
                  entry = cons (name, r1);
                  aa = cons (entry, cell_nil);
                  set_cdr_x (aa, cdr (r0));
                  set_cdr_x (r0, aa);
                  cl = module_variable (r0, cell_closure);
                  set_cdr_x (cl, aa);
                }
              r1 = cell_unspecified;
              goto vm_return;
            }
          push_cc (CAR (r1), r1, r0, cell_vm_eval_check_func);
          gc_check ();
          goto eval;
        eval_check_func:
          push_cc (CDR (r2), r2, r0, cell_vm_eval2);
          goto evlis;
        eval2:
          r1 = cons (CAR (r2), r1);
          goto apply;
        }
    }
  else if (t == TSYMBOL)
    {
      if (r1 == cell_symbol_boot_module)
        goto vm_return;
      if (r1 == cell_symbol_current_module)
        goto vm_return;
      if (r1 == cell_symbol_begin) // FIXME
        {
          r1 = cell_begin;
          goto vm_return;
        }
      r1 = assert_defined (r1, module_ref (r0, r1));
      goto vm_return;
    }
  else if (t == TVARIABLE)
    {
      r1 = CDR (VARIABLE (r1));
      goto vm_return;
    }
  else if (t == TBROKEN_HEART)
    error (cell_symbol_system_error,  r1);
  else
    goto vm_return;

 macro_expand:
  {
    macro;
    expanders;

    if (TYPE (r1) != TPAIR || CAR (r1) == cell_symbol_quote)
      goto vm_return;

    if (CAR (r1) == cell_symbol_lambda)
      {
        push_cc (CDDR (r1), r1, r0, cell_vm_macro_expand_lambda);
        goto macro_expand;
      macro_expand_lambda:
        CDDR (r2) = r1;
        r1 = r2;
        goto vm_return;
      }

    if (TYPE (r1) == TPAIR
        && (macro = get_macro (CAR (r1))) != cell_f)
      {
        r1 = cons (macro, CDR (r1));
        push_cc (r1, cell_nil, r0, cell_vm_macro_expand);
        goto apply;
      }

    if (CAR (r1) == cell_symbol_define
        || CAR (r1) == cell_symbol_define_macro)
      {
        push_cc (CDDR (r1), r1, r0, cell_vm_macro_expand_define);
        goto macro_expand;
      macro_expand_define:
        CDDR (r2) = r1;
        r1 = r2;
        if (CAR (r1) == cell_symbol_define_macro)
          {
            push_cc (r1, r1, r0, cell_vm_macro_expand_define_macro);
            goto eval;
          macro_expand_define_macro:
            r1 = r2;
          }
        goto vm_return;
      }

    if (CAR (r1) == cell_symbol_set_x)
      {
        push_cc (CDDR (r1), r1, r0, cell_vm_macro_expand_set_x);
        goto macro_expand;
      macro_expand_set_x:
        CDDR (r2) = r1;
        r1 = r2;
        goto vm_return;
      }

    if (TYPE (r1) == TPAIR
        && TYPE (CAR (r1)) == TSYMBOL
        && CAR (r1) != cell_symbol_begin
        && ((macro = macro_get_handle (cell_symbol_portable_macro_expand)) != cell_f)
        && ((expanders = module_ref (r0, cell_symbol_sc_expander_alist)) != cell_undefined)
        && ((macro = assq (CAR (r1), expanders)) != cell_f))
      {
        sc_expand = module_ref (r0, cell_symbol_macro_expand);
        r2 = r1;
        if (sc_expand != cell_undefined && sc_expand != cell_f)
          {
            r1 = cons (sc_expand, cons (r1, cell_nil));
            goto apply;
          }
      }

    push_cc (CAR (r1), r1, r0, cell_vm_macro_expand_car);
    goto macro_expand;

  macro_expand_car:
    CAR (r2) = r1;
    r1 = r2;
    if (CDR (r1) == cell_nil)
      goto vm_return;

    push_cc (CDR (r1), r1, r0, cell_vm_macro_expand_cdr);
    goto macro_expand;

  macro_expand_cdr:
    CDR (r2) = r1;
    r1 = r2;

    goto vm_return;
  }

 begin:
  x = cell_unspecified;
  while (r1 != cell_nil)
    {
      gc_check ();
      if (TYPE (r1) == TPAIR)
        {
          if (CAAR (r1) == cell_symbol_primitive_load)
            {
              program = cons (CAR (r1), cell_nil);
              push_cc (program, r1, r0, cell_vm_begin_primitive_load);
              goto begin_expand;
            begin_primitive_load:
              CAR (r2) = r1;
              r1 = r2;
            }
        }

      if (TYPE (r1) == TPAIR && TYPE (CAR (r1)) == TPAIR)
        {
          if (CAAR (r1) == cell_symbol_begin)
            r1 = append2 (CDAR (r1), CDR (r1));
        }
      if (CDR (r1) == cell_nil)
        {
          r1 = CAR (r1);
          goto eval;
        }
      push_cc (CAR (r1), r1, r0, cell_vm_begin_eval);
      goto eval;
    begin_eval:
      x = r1;
      r1 = CDR (r2);
    }
  r1 = x;
  goto vm_return;


 begin_expand:
  x = cell_unspecified;
  while (r1 != cell_nil)
    {
      gc_check ();

      if (TYPE (r1) == TPAIR)
        {
          if (TYPE (CAR (r1)) == TPAIR && CAAR (r1) == cell_symbol_begin)
            r1 = append2 (CDAR (r1), CDR (r1));
          if (CAAR (r1) == cell_symbol_primitive_load)
            {
              push_cc (CADR (CAR (r1)), r1, r0, cell_vm_begin_expand_primitive_load);
              goto eval; // FIXME: expand too?!
            begin_expand_primitive_load:
              if (TYPE (r1) == TNUMBER && VALUE (r1) == 0)
                ;
              else if (TYPE (r1) == TSTRING)
                input = set_current_input_port (open_input_file (r1));
              else if (TYPE (r1) == TPORT)
                input = set_current_input_port (r1);
              else
                assert (0);

              push_cc (input, r2, r0, cell_vm_return);
              x = read_input_file_env (r0);
              if (g_debug > 3)
                module_printer (m0);
              gc_pop_frame ();
              input = r1;
              r1 = x;
              set_current_input_port (input);
              r1 = cons (cell_symbol_begin, r1);
              CAR (r2) = r1;
              r1 = r2;
              continue;
            }
        }

      push_cc (CAR (r1), r1, r0, cell_vm_begin_expand_macro);
      goto macro_expand;
    begin_expand_macro:
      if (r1 != CAR (r2))
        {
          CAR (r2) = r1;
          r1 = r2;
          continue;
        }
      r1 = r2;
      expand_variable (CAR (r1), cell_nil);
      push_cc (CAR (r1), r1, r0, cell_vm_begin_expand_eval);
      goto eval;
    begin_expand_eval:
      x = r1;
      r1 = CDR (r2);
    }
  r1 = x;
  goto vm_return;

 vm_if:
  push_cc (CAR (r1), r1, r0, cell_vm_if_expr);
  goto eval;
 if_expr:
  x = r1;
  r1 = r2;
  if (x != cell_f)
    {
      r1 = CADR (r1);
      goto eval;
    }
  if (CDDR (r1) != cell_nil)
    {
      r1 = CAR (CDDR (r1));
      goto eval;
    }
  r1 = cell_unspecified;
  goto vm_return;

 call_with_current_continuation:
  gc_push_frame ();
  x = MAKE_CONTINUATION (g_continuations++);
  v = make_vector__ (STACK_SIZE-g_stack);
  for (t=g_stack; t < STACK_SIZE; t++)
    vector_set_x_ (v, t-g_stack, g_stack_array[t]);
  CONTINUATION (x) = v;
  gc_pop_frame ();
  push_cc (cons (CAR (r1), cons (x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
  goto apply;
 call_with_current_continuation2:
  v = make_vector__ (STACK_SIZE-g_stack);
  for (t=g_stack; t < STACK_SIZE; t++)
    vector_set_x_ (v, t-g_stack, g_stack_array[t]);
  CONTINUATION (r2) = v;
  goto vm_return;

 call_with_values:
  push_cc (cons (CAR (r1), cell_nil), r1, r0, cell_vm_call_with_values2);
  goto apply;
 call_with_values2:
  if (TYPE (r1) == TVALUES)
    r1 = CDR (r1);
  r1 = cons (CADR (r2), r1);
  goto apply;

 vm_return:
  x = r1;
  gc_pop_frame ();
  r1 = x;
  goto eval_apply;
}

SCM
apply (SCM f, SCM x, SCM a) ///((internal))
{
  push_cc (cons (f, x), cell_unspecified, r0, cell_unspecified);
  r3 = cell_vm_apply;
  return eval_apply ();
}

SCM
mes_g_stack (SCM a) ///((internal))
{
  //g_stack = g_free + ARENA_SIZE;
  g_stack = STACK_SIZE;
  r0 = a;
  r1 = MAKE_CHAR (0);
  r2 = MAKE_CHAR (0);
  r3 = MAKE_CHAR (0);
  return r0;
}

// Environment setup

#include "src/hash.c"
#include "src/module.c"
#include "src/posix.c"
#include "src/math.c"
#include "src/lib.c"

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells () ///((internal))
{
  long arena_bytes = (ARENA_SIZE+JAM_SIZE)*sizeof (struct scm);
  void *p = malloc (arena_bytes+STACK_SIZE*sizeof (SCM));
  g_cells = (struct scm *)p;
  g_stack_array = (SCM*)(p + arena_bytes);

  TYPE (0) = TVECTOR;
  LENGTH (0) = 1000;
  VECTOR (0) = 0;
  g_cells++;
  TYPE (0) = TCHAR;
  VALUE (0) = 'c';
  return 0;
}

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();

#if MES_MINI

g_free++;
g_cells[cell_nil] = scm_nil;

g_free++;
g_cells[cell_f] = scm_f;

g_free++;
g_cells[cell_t] = scm_t;

g_free++;
g_cells[cell_dot] = scm_dot;

g_free++;
g_cells[cell_arrow] = scm_arrow;

g_free++;
g_cells[cell_undefined] = scm_undefined;

g_free++;
g_cells[cell_unspecified] = scm_unspecified;

g_free++;
g_cells[cell_closure] = scm_closure;

g_free++;
g_cells[cell_circular] = scm_circular;

g_free++;
g_cells[cell_begin] = scm_begin;

g_free++;
g_cells[cell_symbol_dot] = scm_symbol_dot;

g_free++;
g_cells[cell_symbol_lambda] = scm_symbol_lambda;

g_free++;
g_cells[cell_symbol_begin] = scm_symbol_begin;

g_free++;
g_cells[cell_symbol_if] = scm_symbol_if;

g_free++;
g_cells[cell_symbol_quote] = scm_symbol_quote;

g_free++;
g_cells[cell_symbol_define] = scm_symbol_define;

g_free++;
g_cells[cell_symbol_define_macro] = scm_symbol_define_macro;

g_free++;
g_cells[cell_symbol_quasiquote] = scm_symbol_quasiquote;

g_free++;
g_cells[cell_symbol_unquote] = scm_symbol_unquote;

g_free++;
g_cells[cell_symbol_unquote_splicing] = scm_symbol_unquote_splicing;

g_free++;
g_cells[cell_symbol_syntax] = scm_symbol_syntax;

g_free++;
g_cells[cell_symbol_quasisyntax] = scm_symbol_quasisyntax;

g_free++;
g_cells[cell_symbol_unsyntax] = scm_symbol_unsyntax;

g_free++;
g_cells[cell_symbol_unsyntax_splicing] = scm_symbol_unsyntax_splicing;

g_free++;
g_cells[cell_symbol_set_x] = scm_symbol_set_x;

g_free++;
g_cells[cell_symbol_sc_expand] = scm_symbol_sc_expand;

g_free++;
g_cells[cell_symbol_macro_expand] = scm_symbol_macro_expand;

g_free++;
g_cells[cell_symbol_portable_macro_expand] = scm_symbol_portable_macro_expand;

g_free++;
g_cells[cell_symbol_sc_expander_alist] = scm_symbol_sc_expander_alist;

g_free++;
g_cells[cell_symbol_call_with_values] = scm_symbol_call_with_values;

g_free++;
g_cells[cell_call_with_current_continuation] = scm_call_with_current_continuation;

g_free++;
g_cells[cell_symbol_call_with_current_continuation] = scm_symbol_call_with_current_continuation;

g_free++;
g_cells[cell_symbol_boot_module] = scm_symbol_boot_module;

g_free++;
g_cells[cell_symbol_current_module] = scm_symbol_current_module;

g_free++;
g_cells[cell_symbol_primitive_load] = scm_symbol_primitive_load;

g_free++;
g_cells[cell_symbol_read_input_file] = scm_symbol_read_input_file;

g_free++;
g_cells[cell_symbol_write] = scm_symbol_write;

g_free++;
g_cells[cell_symbol_display] = scm_symbol_display;

g_free++;
g_cells[cell_symbol_throw] = scm_symbol_throw;

g_free++;
g_cells[cell_symbol_not_a_number] = scm_symbol_not_a_number;

g_free++;
g_cells[cell_symbol_not_a_pair] = scm_symbol_not_a_pair;

g_free++;
g_cells[cell_symbol_system_error] = scm_symbol_system_error;

g_free++;
g_cells[cell_symbol_wrong_number_of_args] = scm_symbol_wrong_number_of_args;

g_free++;
g_cells[cell_symbol_wrong_type_arg] = scm_symbol_wrong_type_arg;

g_free++;
g_cells[cell_symbol_unbound_variable] = scm_symbol_unbound_variable;

g_free++;
g_cells[cell_symbol_hashq_table] = scm_symbol_hashq_table;

g_free++;
g_cells[cell_symbol_record_type] = scm_symbol_record_type;

g_free++;
g_cells[cell_symbol_frame] = scm_symbol_frame;

g_free++;
g_cells[cell_symbol_module] = scm_symbol_module;

g_free++;
g_cells[cell_symbol_stack] = scm_symbol_stack;

g_free++;
g_cells[cell_symbol_buckets] = scm_symbol_buckets;

g_free++;
g_cells[cell_symbol_procedure] = scm_symbol_procedure;

g_free++;
g_cells[cell_symbol_size] = scm_symbol_size;

g_free++;
g_cells[cell_symbol_argv] = scm_symbol_argv;

g_free++;
g_cells[cell_symbol_mes_prefix] = scm_symbol_mes_prefix;

g_free++;
g_cells[cell_symbol_mes_version] = scm_symbol_mes_version;

g_free++;
g_cells[cell_symbol_car] = scm_symbol_car;

g_free++;
g_cells[cell_symbol_cdr] = scm_symbol_cdr;

g_free++;
g_cells[cell_symbol_pmatch_car] = scm_symbol_pmatch_car;

g_free++;
g_cells[cell_symbol_pmatch_cdr] = scm_symbol_pmatch_cdr;

g_free++;
g_cells[cell_vm_evlis] = scm_vm_evlis;

g_free++;
g_cells[cell_vm_evlis2] = scm_vm_evlis2;

g_free++;
g_cells[cell_vm_evlis3] = scm_vm_evlis3;

g_free++;
g_cells[cell_vm_apply] = scm_vm_apply;

g_free++;
g_cells[cell_vm_apply2] = scm_vm_apply2;

g_free++;
g_cells[cell_vm_eval] = scm_vm_eval;

g_free++;
g_cells[cell_vm_eval_pmatch_car] = scm_vm_eval_pmatch_car;

g_free++;
g_cells[cell_vm_eval_pmatch_cdr] = scm_vm_eval_pmatch_cdr;

g_free++;
g_cells[cell_vm_eval_define] = scm_vm_eval_define;

g_free++;
g_cells[cell_vm_eval_set_x] = scm_vm_eval_set_x;

g_free++;
g_cells[cell_vm_eval_macro_expand_eval] = scm_vm_eval_macro_expand_eval;

g_free++;
g_cells[cell_vm_eval_macro_expand_expand] = scm_vm_eval_macro_expand_expand;

g_free++;
g_cells[cell_vm_eval_check_func] = scm_vm_eval_check_func;

g_free++;
g_cells[cell_vm_eval2] = scm_vm_eval2;

g_free++;
g_cells[cell_vm_macro_expand] = scm_vm_macro_expand;

g_free++;
g_cells[cell_vm_macro_expand_define] = scm_vm_macro_expand_define;

g_free++;
g_cells[cell_vm_macro_expand_define_macro] = scm_vm_macro_expand_define_macro;

g_free++;
g_cells[cell_vm_macro_expand_lambda] = scm_vm_macro_expand_lambda;

g_free++;
g_cells[cell_vm_macro_expand_set_x] = scm_vm_macro_expand_set_x;

g_free++;
g_cells[cell_vm_begin_expand_primitive_load] = scm_vm_begin_expand_primitive_load;

g_free++;
g_cells[cell_vm_begin_primitive_load] = scm_vm_begin_primitive_load;

g_free++;
g_cells[cell_vm_macro_expand_car] = scm_vm_macro_expand_car;

g_free++;
g_cells[cell_vm_macro_expand_cdr] = scm_vm_macro_expand_cdr;

g_free++;
g_cells[cell_vm_begin_expand] = scm_vm_begin_expand;

g_free++;
g_cells[cell_vm_begin_expand_eval] = scm_vm_begin_expand_eval;

g_free++;
g_cells[cell_vm_begin_expand_macro] = scm_vm_begin_expand_macro;

g_free++;
g_cells[cell_vm_begin] = scm_vm_begin;

g_free++;
g_cells[cell_vm_begin_read_input_file] = scm_vm_begin_read_input_file;

g_free++;
g_cells[cell_vm_begin_eval] = scm_vm_begin_eval;

g_free++;
g_cells[cell_vm_if] = scm_vm_if;

g_free++;
g_cells[cell_vm_if_expr] = scm_vm_if_expr;

g_free++;
g_cells[cell_vm_call_with_values2] = scm_vm_call_with_values2;

g_free++;
g_cells[cell_vm_call_with_current_continuation2] = scm_vm_call_with_current_continuation2;

g_free++;
g_cells[cell_vm_return] = scm_vm_return;

g_free++;
g_cells[cell_type_bytes] = scm_type_bytes;

g_free++;
g_cells[cell_type_char] = scm_type_char;

g_free++;
g_cells[cell_type_closure] = scm_type_closure;

g_free++;
g_cells[cell_type_continuation] = scm_type_continuation;

g_free++;
g_cells[cell_type_function] = scm_type_function;

g_free++;
g_cells[cell_type_keyword] = scm_type_keyword;

g_free++;
g_cells[cell_type_macro] = scm_type_macro;

g_free++;
g_cells[cell_type_number] = scm_type_number;

g_free++;
g_cells[cell_type_pair] = scm_type_pair;

g_free++;
g_cells[cell_type_port] = scm_type_port;

g_free++;
g_cells[cell_type_ref] = scm_type_ref;

g_free++;
g_cells[cell_type_special] = scm_type_special;

g_free++;
g_cells[cell_type_string] = scm_type_string;

g_free++;
g_cells[cell_type_struct] = scm_type_struct;

g_free++;
g_cells[cell_type_symbol] = scm_type_symbol;

g_free++;
g_cells[cell_type_values] = scm_type_values;

g_free++;
g_cells[cell_type_variable] = scm_type_variable;

g_free++;
g_cells[cell_type_vector] = scm_type_vector;

g_free++;
g_cells[cell_type_broken_heart] = scm_type_broken_heart;

g_free++;
g_cells[cell_symbol_internal_time_units_per_second] = scm_symbol_internal_time_units_per_second;

g_free++;
g_cells[cell_symbol_compiler] = scm_symbol_compiler;

g_free++;
g_cells[cell_symbol_arch] = scm_symbol_arch;

g_free++;
g_cells[cell_test] = scm_test;

#elif !POSIX
#include "src/mes.mes.symbols.i"
#else
#include "src/mes.symbols.i"
#endif

g_symbol_max = g_free++;

#if MES_MINI

#if !POSIX
 #define name cdr
#endif

NAME_SYMBOL (cell_nil, scm_nil.name);
NAME_SYMBOL (cell_f, scm_f.name);
NAME_SYMBOL (cell_t, scm_t.name);
NAME_SYMBOL (cell_dot, scm_dot.name);
NAME_SYMBOL (cell_arrow, scm_arrow.name);
NAME_SYMBOL (cell_undefined, scm_undefined.name);
NAME_SYMBOL (cell_unspecified, scm_unspecified.name);
NAME_SYMBOL (cell_closure, scm_closure.name);
NAME_SYMBOL (cell_circular, scm_circular.name);
NAME_SYMBOL (cell_begin, scm_begin.name);
NAME_SYMBOL (cell_symbol_dot, scm_symbol_dot.name);
NAME_SYMBOL (cell_symbol_lambda, scm_symbol_lambda.name);
NAME_SYMBOL (cell_symbol_begin, scm_symbol_begin.name);
NAME_SYMBOL (cell_symbol_if, scm_symbol_if.name);
NAME_SYMBOL (cell_symbol_quote, scm_symbol_quote.name);
NAME_SYMBOL (cell_symbol_define, scm_symbol_define.name);
NAME_SYMBOL (cell_symbol_define_macro, scm_symbol_define_macro.name);
NAME_SYMBOL (cell_symbol_quasiquote, scm_symbol_quasiquote.name);
NAME_SYMBOL (cell_symbol_unquote, scm_symbol_unquote.name);
NAME_SYMBOL (cell_symbol_unquote_splicing, scm_symbol_unquote_splicing.name);
NAME_SYMBOL (cell_symbol_syntax, scm_symbol_syntax.name);
NAME_SYMBOL (cell_symbol_quasisyntax, scm_symbol_quasisyntax.name);
NAME_SYMBOL (cell_symbol_unsyntax, scm_symbol_unsyntax.name);
NAME_SYMBOL (cell_symbol_unsyntax_splicing, scm_symbol_unsyntax_splicing.name);
NAME_SYMBOL (cell_symbol_set_x, scm_symbol_set_x.name);
NAME_SYMBOL (cell_symbol_sc_expand, scm_symbol_sc_expand.name);
NAME_SYMBOL (cell_symbol_macro_expand, scm_symbol_macro_expand.name);
NAME_SYMBOL (cell_symbol_portable_macro_expand, scm_symbol_portable_macro_expand.name);
NAME_SYMBOL (cell_symbol_sc_expander_alist, scm_symbol_sc_expander_alist.name);
NAME_SYMBOL (cell_symbol_call_with_values, scm_symbol_call_with_values.name);
NAME_SYMBOL (cell_call_with_current_continuation, scm_call_with_current_continuation.name);
NAME_SYMBOL (cell_symbol_call_with_current_continuation, scm_symbol_call_with_current_continuation.name);
NAME_SYMBOL (cell_symbol_boot_module, scm_symbol_boot_module.name);
NAME_SYMBOL (cell_symbol_current_module, scm_symbol_current_module.name);
NAME_SYMBOL (cell_symbol_primitive_load, scm_symbol_primitive_load.name);
NAME_SYMBOL (cell_symbol_read_input_file, scm_symbol_read_input_file.name);
NAME_SYMBOL (cell_symbol_write, scm_symbol_write.name);
NAME_SYMBOL (cell_symbol_display, scm_symbol_display.name);
NAME_SYMBOL (cell_symbol_throw, scm_symbol_throw.name);
NAME_SYMBOL (cell_symbol_not_a_number, scm_symbol_not_a_number.name);
NAME_SYMBOL (cell_symbol_not_a_pair, scm_symbol_not_a_pair.name);
NAME_SYMBOL (cell_symbol_system_error, scm_symbol_system_error.name);
NAME_SYMBOL (cell_symbol_wrong_number_of_args, scm_symbol_wrong_number_of_args.name);
NAME_SYMBOL (cell_symbol_wrong_type_arg, scm_symbol_wrong_type_arg.name);
NAME_SYMBOL (cell_symbol_unbound_variable, scm_symbol_unbound_variable.name);
NAME_SYMBOL (cell_symbol_hashq_table, scm_symbol_hashq_table.name);
NAME_SYMBOL (cell_symbol_record_type, scm_symbol_record_type.name);
NAME_SYMBOL (cell_symbol_frame, scm_symbol_frame.name);
NAME_SYMBOL (cell_symbol_module, scm_symbol_module.name);
NAME_SYMBOL (cell_symbol_stack, scm_symbol_stack.name);
NAME_SYMBOL (cell_symbol_buckets, scm_symbol_buckets.name);
NAME_SYMBOL (cell_symbol_procedure, scm_symbol_procedure.name);
NAME_SYMBOL (cell_symbol_size, scm_symbol_size.name);
NAME_SYMBOL (cell_symbol_argv, scm_symbol_argv.name);
NAME_SYMBOL (cell_symbol_mes_prefix, scm_symbol_mes_prefix.name);
NAME_SYMBOL (cell_symbol_mes_version, scm_symbol_mes_version.name);
NAME_SYMBOL (cell_symbol_car, scm_symbol_car.name);
NAME_SYMBOL (cell_symbol_cdr, scm_symbol_cdr.name);
NAME_SYMBOL (cell_symbol_pmatch_car, scm_symbol_pmatch_car.name);
NAME_SYMBOL (cell_symbol_pmatch_cdr, scm_symbol_pmatch_cdr.name);
NAME_SYMBOL (cell_vm_evlis, scm_vm_evlis.name);
NAME_SYMBOL (cell_vm_evlis2, scm_vm_evlis2.name);
NAME_SYMBOL (cell_vm_evlis3, scm_vm_evlis3.name);
NAME_SYMBOL (cell_vm_apply, scm_vm_apply.name);
NAME_SYMBOL (cell_vm_apply2, scm_vm_apply2.name);
NAME_SYMBOL (cell_vm_eval, scm_vm_eval.name);
NAME_SYMBOL (cell_vm_eval_pmatch_car, scm_vm_eval_pmatch_car.name);
NAME_SYMBOL (cell_vm_eval_pmatch_cdr, scm_vm_eval_pmatch_cdr.name);
NAME_SYMBOL (cell_vm_eval_define, scm_vm_eval_define.name);
NAME_SYMBOL (cell_vm_eval_set_x, scm_vm_eval_set_x.name);
NAME_SYMBOL (cell_vm_eval_macro_expand_eval, scm_vm_eval_macro_expand_eval.name);
NAME_SYMBOL (cell_vm_eval_macro_expand_expand, scm_vm_eval_macro_expand_expand.name);
NAME_SYMBOL (cell_vm_eval_check_func, scm_vm_eval_check_func.name);
NAME_SYMBOL (cell_vm_eval2, scm_vm_eval2.name);
NAME_SYMBOL (cell_vm_macro_expand, scm_vm_macro_expand.name);
NAME_SYMBOL (cell_vm_macro_expand_define, scm_vm_macro_expand_define.name);
NAME_SYMBOL (cell_vm_macro_expand_define_macro, scm_vm_macro_expand_define_macro.name);
NAME_SYMBOL (cell_vm_macro_expand_lambda, scm_vm_macro_expand_lambda.name);
NAME_SYMBOL (cell_vm_macro_expand_set_x, scm_vm_macro_expand_set_x.name);
NAME_SYMBOL (cell_vm_begin_expand_primitive_load, scm_vm_begin_expand_primitive_load.name);
NAME_SYMBOL (cell_vm_begin_primitive_load, scm_vm_begin_primitive_load.name);
NAME_SYMBOL (cell_vm_macro_expand_car, scm_vm_macro_expand_car.name);
NAME_SYMBOL (cell_vm_macro_expand_cdr, scm_vm_macro_expand_cdr.name);
NAME_SYMBOL (cell_vm_begin_expand, scm_vm_begin_expand.name);
NAME_SYMBOL (cell_vm_begin_expand_eval, scm_vm_begin_expand_eval.name);
NAME_SYMBOL (cell_vm_begin_expand_macro, scm_vm_begin_expand_macro.name);
NAME_SYMBOL (cell_vm_begin, scm_vm_begin.name);
NAME_SYMBOL (cell_vm_begin_read_input_file, scm_vm_begin_read_input_file.name);
NAME_SYMBOL (cell_vm_begin_eval, scm_vm_begin_eval.name);
NAME_SYMBOL (cell_vm_if, scm_vm_if.name);
NAME_SYMBOL (cell_vm_if_expr, scm_vm_if_expr.name);
NAME_SYMBOL (cell_vm_call_with_values2, scm_vm_call_with_values2.name);
NAME_SYMBOL (cell_vm_call_with_current_continuation2, scm_vm_call_with_current_continuation2.name);
NAME_SYMBOL (cell_vm_return, scm_vm_return.name);
NAME_SYMBOL (cell_type_bytes, scm_type_bytes.name);
NAME_SYMBOL (cell_type_char, scm_type_char.name);
NAME_SYMBOL (cell_type_closure, scm_type_closure.name);
NAME_SYMBOL (cell_type_continuation, scm_type_continuation.name);
NAME_SYMBOL (cell_type_function, scm_type_function.name);
NAME_SYMBOL (cell_type_keyword, scm_type_keyword.name);
NAME_SYMBOL (cell_type_macro, scm_type_macro.name);
NAME_SYMBOL (cell_type_number, scm_type_number.name);
NAME_SYMBOL (cell_type_pair, scm_type_pair.name);
NAME_SYMBOL (cell_type_port, scm_type_port.name);
NAME_SYMBOL (cell_type_ref, scm_type_ref.name);
NAME_SYMBOL (cell_type_special, scm_type_special.name);
NAME_SYMBOL (cell_type_string, scm_type_string.name);
NAME_SYMBOL (cell_type_struct, scm_type_struct.name);
NAME_SYMBOL (cell_type_symbol, scm_type_symbol.name);
NAME_SYMBOL (cell_type_values, scm_type_values.name);
NAME_SYMBOL (cell_type_variable, scm_type_variable.name);
NAME_SYMBOL (cell_type_vector, scm_type_vector.name);
NAME_SYMBOL (cell_type_broken_heart, scm_type_broken_heart.name);
NAME_SYMBOL (cell_symbol_internal_time_units_per_second, scm_symbol_internal_time_units_per_second.name);
NAME_SYMBOL (cell_symbol_compiler, scm_symbol_compiler.name);
NAME_SYMBOL (cell_symbol_arch, scm_symbol_arch.name);
NAME_SYMBOL (cell_test, scm_test.name);

#if !POSIX
 #undef name
#endif

#elif !POSIX
#include "src/mes.mes.symbol-names.i"
#else
#include "src/mes.symbol-names.i"
#endif

  g_symbols = make_hash_table_ (500);
  for (int i=1; i<g_symbol_max; i++)
    hash_set_x (g_symbols, symbol_to_string (i), i);

  SCM a = cell_nil;
  a = acons (cell_symbol_call_with_values, cell_symbol_call_with_values, a);
  a = acons (cell_symbol_boot_module, cell_symbol_boot_module, a);
  a = acons (cell_symbol_current_module, cell_symbol_current_module, a);
  a = acons (cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);

  a = acons (cell_symbol_mes_version, MAKE_STRING0 (VERSION), a);
  a = acons (cell_symbol_mes_prefix, MAKE_STRING0 (PREFIX), a);

  a = acons (cell_type_bytes, MAKE_NUMBER (TBYTES), a);
  a = acons (cell_type_char, MAKE_NUMBER (TCHAR), a);
  a = acons (cell_type_closure, MAKE_NUMBER (TCLOSURE), a);
  a = acons (cell_type_continuation, MAKE_NUMBER (TCONTINUATION), a);
  a = acons (cell_type_function, MAKE_NUMBER (TFUNCTION), a);
  a = acons (cell_type_keyword, MAKE_NUMBER (TKEYWORD), a);
  a = acons (cell_type_macro, MAKE_NUMBER (TMACRO), a);
  a = acons (cell_type_number, MAKE_NUMBER (TNUMBER), a);
  a = acons (cell_type_pair, MAKE_NUMBER (TPAIR), a);
  a = acons (cell_type_port, MAKE_NUMBER (TPORT), a);
  a = acons (cell_type_ref, MAKE_NUMBER (TREF), a);
  a = acons (cell_type_special, MAKE_NUMBER (TSPECIAL), a);
  a = acons (cell_type_string, MAKE_NUMBER (TSTRING), a);
  a = acons (cell_type_struct, MAKE_NUMBER (TSTRUCT), a);
  a = acons (cell_type_symbol, MAKE_NUMBER (TSYMBOL), a);
  a = acons (cell_type_values, MAKE_NUMBER (TVALUES), a);
  a = acons (cell_type_variable, MAKE_NUMBER (TVARIABLE), a);
  a = acons (cell_type_vector, MAKE_NUMBER (TVECTOR), a);
  a = acons (cell_type_broken_heart, MAKE_NUMBER (TBROKEN_HEART), a);

  a = acons (cell_closure, a, a);

  return a;
}

SCM
mes_environment (int argc, char *argv[])
{
  SCM a = mes_symbols ();

  char *compiler = "gnuc";
#if __MESC__
  compiler = "mesc";
#elif __TINYC__
  compiler = "tcc";
#endif
  a = acons (cell_symbol_compiler, MAKE_STRING0 (compiler), a);

  char *arch = "x86";
#if __x86_64__
  arch = "x86_64";
#endif
  a = acons (cell_symbol_arch, MAKE_STRING0 (arch), a);

#if !MES_MINI
  SCM lst = cell_nil;
  for (int i=argc-1; i>=0; i--)
    lst = cons (MAKE_STRING0 (argv[i]), lst);
  a = acons (cell_symbol_argv, lst, a);
#endif

  return mes_g_stack (a);
}

SCM
mes_builtins (SCM a) ///((internal))
{
#if MES_MINI

#if !POSIX
 #define function car
#endif

//mes
scm_cons.function = g_function;
g_functions[g_function++] = fun_cons;
cell_cons = g_free++;
g_cells[cell_cons] = scm_cons;

scm_car.function = g_function;
g_functions[g_function++] = fun_car;
cell_car = g_free++;
g_cells[cell_car] = scm_car;

scm_cdr.function = g_function;
g_functions[g_function++] = fun_cdr;
cell_cdr = g_free++;
g_cells[cell_cdr] = scm_cdr;

scm_list.function = g_function;
g_functions[g_function++] = fun_list;
cell_list = g_free++;
g_cells[cell_list] = scm_list;

scm_null_p.function = g_function;
g_functions[g_function++] = fun_null_p;
cell_null_p = g_free++;
g_cells[cell_null_p] = scm_null_p;

scm_eq_p.function = g_function;
g_functions[g_function++] = fun_eq_p;
cell_eq_p = g_free++;
g_cells[cell_eq_p] = scm_eq_p;

//math
scm_minus.function = g_function;
g_functions[g_function++] = fun_minus;
cell_minus = g_free++;
g_cells[cell_minus] = scm_minus;

scm_plus.function = g_function;
g_functions[g_function++] = fun_plus;
cell_plus = g_free++;
g_cells[cell_plus] = scm_plus;

//lib
scm_display_.function = g_function;
g_functions[g_function++] = fun_display_;
cell_display_ = g_free++;
g_cells[cell_display_] = scm_display_;

scm_display_error_.function = g_function;
g_functions[g_function++] = fun_display_error_;
cell_display_error_ = g_free++;
g_cells[cell_display_error_] = scm_display_error_;

//posix
scm_getenv_.function = g_function;
g_functions[g_function++] = fun_getenv_;
cell_getenv_ = g_free++;
g_cells[cell_getenv_] = scm_getenv_;

#if !POSIX
 #undef name
 #define string cdr
#endif

//mes.environment
scm_cons.string = MAKE_BYTES0 (fun_cons.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_cons)), cell_cons, a);

scm_car.string = MAKE_BYTES0 (fun_car.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_car)), cell_car, a);

scm_cdr.string = MAKE_BYTES0 (fun_cdr.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_cdr)), cell_cdr, a);

scm_list.string = MAKE_BYTES0 (fun_list.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_list)), cell_list, a);

scm_null_p.string = MAKE_BYTES0 (fun_null_p.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_null_p)), cell_null_p, a);

scm_eq_p.string = MAKE_BYTES0 (fun_eq_p.name);
 a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_eq_p)), cell_eq_p, a);

//math.environment
scm_minus.string = MAKE_BYTES0 (fun_minus.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_minus)), cell_minus, a);

scm_plus.string = MAKE_BYTES0 (fun_plus.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_plus)), cell_plus, a);

//lib.environment
scm_display_.string = MAKE_BYTES0 (fun_display_.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_display_)), cell_display_, a);

scm_display_error_.string = MAKE_BYTES0 (fun_display_error_.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_display_error_)), cell_display_error_, a);

//posix.environment
scm_getenv_.string = MAKE_BYTES0 (fun_getenv_.name);
a = acons (cstring_to_symbol (CSTRING_STRUCT (scm_getenv_)), cell_getenv_, a);

#if !POSIX
 #undef function
 #undef string
#endif

#elif !__GNUC__ || !POSIX
#include "src/mes.mes.i"

  // Do not sort: Order of these includes define builtins
#include "src/hash.mes.i"
#include "src/module.mes.i"
#include "src/posix.mes.i"
#include "src/math.mes.i"
#include "src/lib.mes.i"
#include "src/vector.mes.i"
#include "src/strings.mes.i"
#include "src/struct.mes.i"
#include "src/gc.mes.i"
#include "src/reader.mes.i"

#include "src/gc.mes.environment.i"
#include "src/hash.mes.environment.i"
#include "src/lib.mes.environment.i"
#include "src/math.mes.environment.i"
#include "src/mes.mes.environment.i"
#include "src/module.mes.environment.i"
#include "src/posix.mes.environment.i"
#include "src/reader.mes.environment.i"
#include "src/strings.mes.environment.i"
#include "src/struct.mes.environment.i"
#include "src/vector.mes.environment.i"
#else
#include "src/mes.i"

  // Do not sort: Order of these includes define builtins
#include "src/hash.i"
#include "src/module.i"
#include "src/posix.i"
#include "src/math.i"
#include "src/lib.i"
#include "src/vector.i"
#include "src/strings.i"
#include "src/struct.i"
#include "src/gc.i"
#include "src/reader.i"

#include "src/gc.environment.i"
#include "src/hash.environment.i"
#include "src/lib.environment.i"
#include "src/math.environment.i"
#include "src/mes.environment.i"
#include "src/module.environment.i"
#include "src/posix.environment.i"
#include "src/reader.environment.i"
#include "src/strings.environment.i"
#include "src/struct.environment.i"
#include "src/vector.environment.i"
#endif

  if (g_debug > 3)
    {
      fdputs ("functions: ", g_stderr);
      fdputs (itoa (g_function), g_stderr);
      fdputs ("\n", g_stderr);
      for (int i = 0; i < g_function; i++)
        {
          fdputs ("[", g_stderr);
          fdputs (itoa (i), g_stderr);
          fdputs ("]: ", g_stderr);
          fdputs (g_functions[i].name, g_stderr);
          fdputs ("\n", g_stderr);
        }
      fdputs ("\n", g_stderr);
    }

  return a;
}

SCM read_input_file_env (SCM);

int
load_boot (char *prefix, char const *boot, char const *location)
{
  strcpy (prefix + strlen (prefix), boot);
  if (g_debug > 1)
    {
      eputs ("mes: reading boot-0 [");
      eputs (location);
      eputs ("]: ");
      eputs (prefix);
      eputs ("\n");
    }
  int fd = open (prefix, O_RDONLY);
  if (g_debug && fd > 0)
    {
      eputs ("mes: read boot-0: ");
      eputs (prefix);
      eputs ("\n");
    }
  return fd;
}

SCM
load_env () ///((internal))
{
  g_stdin = -1;
  char prefix[1024];
  char boot[1024];
  if (getenv ("MES_BOOT"))
    strcpy (boot, getenv ("MES_BOOT"));
  else
    strcpy (boot, "boot-0.scm");
  if (getenv ("MES_PREFIX"))
    {
      strcpy (prefix, getenv ("MES_PREFIX"));
      strcpy (prefix + strlen (prefix), "/module");
      strcpy (prefix + strlen (prefix), "/mes/");
      g_stdin = load_boot (prefix, boot, "MES_PREFIX");
    }
  if (g_stdin < 0)
    {
      char const *p = MODULEDIR "/mes/";
      strcpy (prefix, p);
      g_stdin = load_boot (prefix, boot, "MODULEDIR");
    }
  if (g_stdin < 0)
    {
      strcpy (prefix, "mes/module/mes/");
      g_stdin = load_boot (prefix, boot, ".");
    }
  if (g_stdin < 0)
    {
      prefix[0] = 0;
      g_stdin = load_boot (prefix, boot, "<boot>");
    }
  if (g_stdin < 0)
    {
      eputs ("mes: boot failed: no such file: ");
      eputs (boot);
      eputs ("\n");
      exit (1);
    }

  r2 = read_input_file_env (r0);
  g_stdin = STDIN;
  return r2;
}

SCM
bload_env () ///((internal))
{
#if !POSIX
  char *mo = "mes/boot-0.32-mo";
  g_stdin = open ("module/mes/boot-0.32-mo", O_RDONLY);
  char *read0 = MODULEDIR "/mes/boot-0.32-mo";
  g_stdin = g_stdin >= 0 ? g_stdin : open (read0, O_RDONLY);
#else
  char *mo ="mes/boot-0.mo";
  g_stdin = open ("module/mes/boot-0.mo", O_RDONLY);
  g_stdin = g_stdin >= 0 ? g_stdin : open (MODULEDIR "/mes/boot-0.mo", O_RDONLY);
#endif

  if (g_stdin < 0)
    {
      eputs ("no such file: ");
      eputs (mo);
      eputs ("\n");
      return 1;
    }
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');

  if (g_debug)
    eputs ("*GOT MES*\n");
  g_stack = getchar () << 8;
  g_stack += getchar ();

  char *p = (char*)g_cells;
  int c = getchar ();
  while (c != EOF)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p-(char*)g_cells) / sizeof (struct scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  // SCM a = struct_ref (r0, 4);
  // a = mes_builtins (a);
  // struct_set_x (r0, 4, a);
  r0 = mes_builtins (r0);

  if (g_debug > 3)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
      eputs ("\n");
      eputs ("functions: ");
      eputs (itoa (g_function));
      eputs ("\n");
      for (int i = 0; i < g_function; i++)
        {
          eputs ("[");
          eputs (itoa (i));
          eputs ("]: ");
          eputs (g_functions[i].name);
          eputs ("\n");
        }
    }
  return r2;
}

#include "src/vector.c"
#include "src/strings.c"
#include "src/struct.c"
#include "src/gc.c"
#include "src/reader.c"

int
main (int argc, char *argv[])
{
  char *p;
  if (p = getenv ("MES_DEBUG"))
    g_debug = atoi (p);
  if (g_debug > 1)
    {
      eputs (";;; MODULEDIR=");
      eputs (MODULEDIR);
      eputs ("\n");
    }
  if (p = getenv ("MES_MAX_ARENA"))
    MAX_ARENA_SIZE = atoi (p);
  if (p = getenv ("MES_ARENA"))
    ARENA_SIZE = atoi (p);
  JAM_SIZE = ARENA_SIZE / 10;
  if (p = getenv ("MES_JAM"))
    JAM_SIZE = atoi (p);
  GC_SAFETY = ARENA_SIZE / 100;
  if (p = getenv ("MES_SAFETY"))
    GC_SAFETY = atoi (p);
  if (p = getenv ("MES_STACK"))
    STACK_SIZE = atoi (p);
  g_stdin = STDIN;
  g_stdout = STDOUT;
  g_stderr = STDERR;

  SCM a = mes_environment (argc, argv);
  a = mes_builtins (a);
  a = init_time (a);
  m0 = make_initial_module (a);
  g_macros = make_hash_table_ (0);

  if (g_debug > 3)
    module_printer (m0);

  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env () : load_env ();
  g_tiny = argc > 2 && !strcmp (argv[2], "--tiny");
  if (argc > 1 && !strcmp (argv[1], "--dump"))
    return dump ();

  push_cc (r2, cell_unspecified, r0, cell_unspecified);

  if (g_debug > 2)
    {
      eputs ("\ngc stats: [");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
  if (g_debug > 3)
    {
      eputs ("program: ");
      write_error_ (r1);
      eputs ("\n");
    }
  // if (g_debug > 3)
  //   {
  //     eputs ("symbols: ");
  //     write_error_ (g_symbols);
  //     eputs ("\n");
  //   }
  r3 = cell_vm_begin_expand;
  r1 = eval_apply ();
  if (g_debug)
    {
      write_error_ (r1);
      eputs ("\n");
    }
  if (g_debug)
    {
      if (g_debug > 3)
        module_printer (m0);

      eputs ("\ngc stats: [");
      eputs (itoa (g_free));
      MAX_ARENA_SIZE = 0;

      gc (g_stack);
      eputs (" => ");
      eputs (itoa (g_free));
      eputs ("]\n");
      if (g_debug > 3)
        module_printer (m0);
      eputs ("\n");

      gc (g_stack);
      eputs (" => ");
      eputs (itoa (g_free));
      eputs ("]\n");
      if (g_debug > 3)
        module_printer (m0);
      eputs ("\n");

      gc (g_stack);
      eputs (" => ");
      eputs (itoa (g_free));
      eputs ("]\n");
      if (g_debug > 3)
        module_printer (m0);
      if (g_debug > 3)
        {
          eputs ("ports:"); write_error_ (g_ports); eputs ("\n");
        }
      eputs ("\n");


    }
  return 0;
}
