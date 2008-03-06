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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*/

void main() {
  char a;
  unsigned char b;
  short c;
  unsigned short d;
  int e;
  unsigned int f;
  char* ptr;
  
  a = a + b;
  a = a + c;
  a = a + d;
  a = a + e;
  a = a + f;
  a = b + c;
  a = b + d;
  a = b + e;
  a = b + f;
  a = c + d;
  a = c + e;
  a = c + f;
  a = d + e;
  a = d + f;
  a = e + f;

  b = a + b;
  b = a + c;
  b = a + d;
  b = a + e;
  b = a + f;
  b = b + c;
  b = b + d;
  b = b + e;
  b = b + f;
  b = c + d;
  b = c + e;
  b = c + f;
  b = d + e;
  b = d + f;
  b = e + f;

  c = a + b;
  c = a + c;
  c = a + d;
  c = a + e;
  c = a + f;
  c = b + c;
  c = b + d;
  c = b + e;
  c = b + f;
  c = c + d;
  c = c + e;
  c = c + f;
  c = d + e;
  c = d + f;
  c = e + f;

  d = a + b;
  d = a + c;
  d = a + d;
  d = a + e;
  d = a + f;
  d = b + c;
  d = b + d;
  d = b + e;
  d = b + f;
  d = c + d;
  d = c + e;
  d = c + f;
  d = d + e;
  d = d + f;
  d = e + f;

  e = a + b;
  e = a + c;
  e = a + d;
  e = a + e;
  e = a + f;
  e = b + c;
  e = b + d;
  e = b + e;
  e = b + f;
  e = c + d;
  e = c + e;
  e = c + f;
  e = d + e;
  e = d + f;
  e = e + f;

  f = a + b;
  f = a + c;
  f = a + d;
  f = a + e;
  f = a + f;
  f = b + c;
  f = b + d;
  f = b + e;
  f = b + f;
  f = c + d;
  f = c + e;
  f = c + f;
  f = d + e;
  f = d + f;
  f = e + f;

  ptr = ptr + a;
  ptr = ptr + b;
  ptr = ptr + c;
  ptr = ptr + d;
  ptr = ptr + e;
  ptr = ptr + f;
}
