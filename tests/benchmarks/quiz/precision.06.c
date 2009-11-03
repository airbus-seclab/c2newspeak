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

char t1[10];
char t2[20];

int main() {
  int rnd;
  int error;
  char *p;
  int n;
  int i;
  error = 0;
  switch (rnd) {
  case 0:
    p = &t1[0];
    n = 10;
    break;
  case 1:
    p = &t2[0];
    n = 20;
    break;
  default:
    error = 1;
    break;
  }
  if (error == 0) {
    for (i = 0; i < n; i++) {
      // precision: should not signal any array out of bounds here:
      p[i] = 1;
    }
  }
  return 0;
}
