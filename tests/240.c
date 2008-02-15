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

void main() {
  char a;
  unsigned char b;
  short c;
  unsigned short d;
  int e;
  unsigned int f;
  long g;
  unsigned long h;
  int* i;
  float j;
  double k;

  int x;
  
  x = a > a;
  x = a > b;
  x = a > c;
  x = a > d;
  x = a > e;
  x = a > f;
  x = a > g;
  x = a > h;
  // x = a > i;  // not allowed, even with --castor
  x = a > j;
  x = a > k;

  x = b > a;
  x = b > b;
  x = b > c;
  x = b > d;
  x = b > e;
  x = b > f;
  x = b > g;
  x = b > h;
  //  x = b > i;  // not allowed, even with --castor
  x = b > j;
  x = b > k;

  x = c > a;
  x = c > b;
  x = c > c;
  x = c > d;
  x = c > e;
  x = c > f;
  x = c > g;
  x = c > h;
  //  x = c > i;  // not allowed, even with --castor
  x = c > j;
  x = c > k;

  x = d > a;
  x = d > b;
  x = d > c;
  x = d > d;
  x = d > e;
  x = d > f;
  x = d > g;
  x = d > h;
  //  x = d > i;  // not allowed, even with --castor
  x = d > j;
  x = d > k;

  x = e > a;
  x = e > b;
  x = e > c;
  x = e > d;
  x = e > e;
  x = e > f;
  x = e > g;
  x = e > h;
  x = e > i;
  x = e > j;
  x = e > k;

  x = f > a;
  x = f > b;
  x = f > c;
  x = f > d;
  x = f > e;
  x = f > f;
  x = f > g;
  x = f > h;
  x = f > i;
  x = f > j;
  x = f > k;

  x = g > a;
  x = g > b;
  x = g > c;
  x = g > d;
  x = g > e;
  x = g > f;
  x = g > g;
  x = g > h;
  x = g > i;
  x = g > j;
  x = g > k;

  x = h > a;
  x = h > b;
  x = h > c;
  x = h > d;
  x = h > e;
  x = h > f;
  x = h > g;
  x = h > h;
  x = h > i;
  x = h > j;
  x = h > k;

  //  x = i > a;  // not allowed, even with --castor
  //  x = i > b;  // not allowed, even with --castor
  //  x = i > c;  // not allowed, even with --castor
  //  x = i > d;  // not allowed, even with --castor
  x = i > e;
  x = i > f;
  x = i > g;
  x = i > h;
  x = i > i;
  //  x = i > j;  // forbidden by gcc
  //  x = i > k;  // forbidden by gcc

  x = j > a;
  x = j > b;
  x = j > c;
  x = j > d;
  x = j > e;
  x = j > f;
  x = j > g;
  x = j > h;
  //  x = j > i;  // forbidden by gcc
  x = j > j;
  x = j > k;

  x = k > a;
  x = k > b;
  x = k > c;
  x = k > d;
  x = k > e;
  x = k > f;
  x = k > g;
  x = k > h;
  //  x = k > i;  // forbidden by gcc
  x = k > j;
  x = k > k;
}
