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

typedef struct {
  void (*f1)();
  void (*f2)();
} S;

void set_f1(S* s, void (*f1)()) {
  s->f1 = f1;
}

void set_f2(S* s, void (*f2)()) {
  s->f2 = f2;
}

void g() {
}

void h() {
  char t[10];
  // precision: should not signal array out of bounds
  t[199] = 1;
}

int main() {
  S x;
  set_f1(&x, &g);
  set_f2(&x, &h);
  
  (*x.f1)();

  return 1;
}
