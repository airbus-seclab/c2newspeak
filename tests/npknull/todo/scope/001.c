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

// should not fail with Fatal error: exception Exceptions.NotImplemented("Store.contains:  L.0 -> H.9 H.9 -> H.2 &&  L.0 -> H.9 H.9 -> H.31")

char t[10];

typedef struct {
  int *a;
  char b[10];
} S;

S y;

void k(char *ptr)
{
  while (1) {
    *ptr = 0;
    ptr++;
  }
}

void f(char* ptr) {
  while (1) {
    *ptr = 0;
    ptr++;
  }
}

void g(int* y) {
  k(y);
}

void h(int* y) {
  f(y);
}

void main(void) {
  int x;

  y.a = &t[0];
  k(y.b);

  f(&x);
  g(&x);
  
  h(&x);
}
