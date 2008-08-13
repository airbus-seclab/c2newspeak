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
  long g;
  unsigned long h;
  
  a = a + b;
  a = a + c;
  a = a + d;
  a = a + e;
  a = a + f;
  a = a + g;
  a = a + h;
  a = b + c;
  a = b + d;
  a = b + e;
  a = b + f;
  a = b + g;
  a = b + h;
  a = c + d;
  a = c + e;
  a = c + f;
  a = c + g;
  a = c + h;
  a = d + e;
  a = d + f;
  a = d + g;
  a = d + h;
  a = e + f;
  a = e + g;
  a = e + h;
  a = f + g;
  a = f + h;
  a = g + h;
}
