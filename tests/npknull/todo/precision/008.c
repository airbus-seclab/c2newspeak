/*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*/


// representative of a case where a costly resource is initialized only when 
// needed
// an alternative is where the pointer resource is used as the boolean too

int init;
char a;
char *resource;

char *get_resource() {
  if (!init) {
    init = 1;
    resource = &a;
  }
  return resource;
}

void main() {
  char *ptr;
  init = 0;

  ptr = get_resource();
  *ptr = 1;                   // precision: should not signal any null pointer
  ptr = get_resource();
  *ptr = 1;                   // precision: should not signal any null pointer
}
