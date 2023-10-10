/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2022,2023 Timothy Sample <samplet@ngyro.com>
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

/* NOTE: This file gets included directly into the body of the
   'mes_builtins' procedure.  */

a = init_builtin (builtin_type, "getpid", 0, &getpid_, a);
a = init_builtin (builtin_type, "environ", -1, &environ_, a);
a = init_builtin (builtin_type, "opendir", 1, &opendir_, a);
a = init_builtin (builtin_type, "closedir", 1, &closedir_, a);
a = init_builtin (builtin_type, "readdir", 1, &readdir_, a);
a = init_builtin (builtin_type, "pipe", 0, &pipe_, a);
a = init_builtin (builtin_type, "close-port", 1, &close_port, a);
a = init_builtin (builtin_type, "seek", 3, &seek, a);
a = init_builtin (builtin_type, "chdir", 1, &chdir_, a);
a = init_builtin (builtin_type, "stat", -1, &stat_, a);
a = init_builtin (builtin_type, "lstat", -1, &lstat_, a);
a = init_builtin (builtin_type, "rename-file", 2, &rename_file, a);
a = init_builtin (builtin_type, "mkdir", 1, &mkdir_, a);
a = init_builtin (builtin_type, "rmdir", 1, &rmdir_, a);
a = init_builtin (builtin_type, "link", 2, &link_, a);
a = init_builtin (builtin_type, "symlink", 2, &symlink_, a);
a = init_builtin (builtin_type, "umask", -1, &umask_, a);
a = init_builtin (builtin_type, "utime", 3, &utime_, a);
a = init_builtin (builtin_type, "sleep", 1, &sleep_, a);

a = acons (cstring_to_symbol ("O_RDWR"), make_number (O_RDWR), a);
a = acons (cstring_to_symbol ("O_EXCL"), make_number (O_EXCL), a);
a = acons (cstring_to_symbol ("O_APPEND"), make_number (O_APPEND), a);

a = acons (cstring_to_symbol ("SEEK_SET"), make_number (SEEK_SET), a);
a = acons (cstring_to_symbol ("SEEK_CUR"), make_number (SEEK_CUR), a);
a = acons (cstring_to_symbol ("SEEK_END"), make_number (SEEK_END), a);

a = acons (cstring_to_symbol ("WNOHANG"), make_number (WNOHANG), a);
