/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#ifndef __MES_BUILTINS_H
#define __MES_BUILTINS_H

/* src/builtins.c */
SCM make_builtin (SCM builtin_type, SCM name, SCM arity, SCM function);
SCM builtin_name (SCM builtin);
SCM builtin_arity (SCM builtin);
SCM builtin_p (SCM x);
SCM builtin_printer (SCM builtin);
/* src/core.c */
SCM car (SCM x);
SCM cdr (SCM x);
SCM list (SCM x);
SCM null_p (SCM x);
SCM eq_p (SCM x, SCM y);
SCM values (SCM x);
SCM acons (SCM key, SCM value, SCM alist);
SCM length (SCM x);
SCM error (SCM key, SCM x);
SCM append2 (SCM x, SCM y);
SCM append_reverse (SCM x, SCM y);
SCM reverse_x_ (SCM x, SCM t);
SCM assq (SCM x, SCM a);
SCM assoc (SCM x, SCM a);
/* src/display.c */
SCM display_ (SCM x);
SCM display_error_ (SCM x);
SCM display_port_ (SCM x, SCM p);
SCM write_ (SCM x);
SCM write_error_ (SCM x);
SCM write_port_ (SCM x, SCM p);
/* src/eval-apply.c */
SCM pairlis (SCM x, SCM y, SCM a);
SCM set_car_x (SCM x, SCM e);
SCM set_cdr_x (SCM x, SCM e);
SCM set_env_x (SCM x, SCM e, SCM a);
SCM add_formals (SCM formals, SCM x);
SCM eval_apply ();
/* src/gc.c */
SCM cons (SCM x, SCM y);
SCM gc_check ();
SCM gc ();
/* src/hash.c */
SCM hashq (SCM x, SCM size);
SCM hash (SCM x, SCM size);
SCM hashq_get_handle (SCM table, SCM key, SCM dflt);
SCM hashq_ref (SCM table, SCM key, SCM dflt);
SCM hash_ref (SCM table, SCM key, SCM dflt);
SCM hashq_set_x (SCM table, SCM key, SCM value);
SCM hash_set_x (SCM table, SCM key, SCM value);
SCM hash_table_printer (SCM table);
SCM make_hash_table (SCM x);
/* src/lib.c */
SCM type_ (SCM x);
SCM car_ (SCM x);
SCM cdr_ (SCM x);
SCM xassq (SCM x, SCM a);
SCM memq (SCM x, SCM a);
SCM equal2_p (SCM a, SCM b);
SCM last_pair (SCM x);
SCM pair_p (SCM x);
SCM char_to_integer (SCM x);
SCM integer_to_char (SCM x);
/* src/math.c */
SCM greater_p (SCM x);
SCM less_p (SCM x);
SCM is_p (SCM x);
SCM minus (SCM x);
SCM plus (SCM x);
SCM divide (SCM x);
SCM modulo (SCM a, SCM b);
SCM multiply (SCM x);
SCM logand (SCM x);
SCM logior (SCM x);
SCM lognot (SCM x);
SCM logxor (SCM x);
SCM ash (SCM n, SCM count);
/* src/module.c */
SCM make_module_type ();
SCM module_printer (SCM module);
SCM module_variable (SCM module, SCM name);
SCM module_ref (SCM module, SCM name);
SCM module_define_x (SCM module, SCM name, SCM value);
/* src/posix.c */
SCM exit_ (SCM x);
SCM peek_byte ();
SCM read_byte ();
SCM unread_byte (SCM i);
SCM peek_char ();
SCM read_char (SCM port);
SCM unread_char (SCM i);
SCM write_char (SCM i);
SCM write_byte (SCM x);
SCM getenv_ (SCM s);
SCM setenv_ (SCM s, SCM v);
SCM access_p (SCM file_name, SCM mode);
SCM current_input_port ();
SCM open_input_file (SCM file_name);
SCM open_input_string (SCM string);
SCM set_current_input_port (SCM port);
SCM current_output_port ();
SCM current_error_port ();
SCM open_output_file (SCM x);
SCM set_current_output_port (SCM port);
SCM set_current_error_port (SCM port);
SCM chmod_ (SCM file_name, SCM mode);
SCM isatty_p (SCM port);
SCM primitive_fork ();
SCM execl_ (SCM file_name, SCM args);
SCM waitpid_ (SCM pid, SCM options);
SCM current_time ();
SCM gettimeofday_ ();
SCM get_internal_run_time ();
SCM getcwd_ ();
SCM dup_ (SCM port);
SCM dup2_ (SCM old, SCM new);
SCM delete_file (SCM file_name);
/* src/reader.c */
SCM read_input_file_env_ (SCM e, SCM a);
SCM read_input_file_env (SCM a);
SCM read_env (SCM a);
SCM reader_read_sexp (SCM c, SCM s, SCM a);
SCM reader_read_character ();
SCM reader_read_binary ();
SCM reader_read_octal ();
SCM reader_read_hex ();
SCM reader_read_string ();
/* src/stack.c */
SCM frame_printer (SCM frame);
SCM make_stack (SCM stack);
SCM stack_length (SCM stack);
SCM stack_ref (SCM stack, SCM index);
/* src/string.c */
SCM string_equal_p (SCM a, SCM b);
SCM symbol_to_string (SCM symbol);
SCM symbol_to_keyword (SCM symbol);
SCM keyword_to_string (SCM keyword);
SCM string_to_symbol (SCM string);
SCM make_symbol (SCM string);
SCM string_to_list (SCM string);
SCM list_to_string (SCM list);
SCM read_string (SCM port);
SCM string_append (SCM x);
SCM string_length (SCM string);
SCM string_ref (SCM str, SCM k);
/* src/struct.c */
SCM make_struct (SCM type, SCM fields, SCM printer);
SCM struct_length (SCM x);
SCM struct_ref (SCM x, SCM i);
SCM struct_set_x (SCM x, SCM i, SCM e);
/* src/vector.c */
SCM make_vector (SCM x);
SCM vector_length (SCM x);
SCM vector_ref (SCM x, SCM i);
SCM vector_entry (SCM x);
SCM vector_set_x (SCM x, SCM i, SCM e);
SCM list_to_vector (SCM x);
SCM vector_to_list (SCM v);

#endif /* __MES_BUILTINS_H */
