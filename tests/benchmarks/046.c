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

extern void read(char *data);

typedef struct {
  int a;
  int b;
} S;

S result[10];

void parse(int *data) {
  int i;
  int n;
  
  // thanks to this test
  if (*data < 0) return;
  if (*data > 10) return;
  n = *data;
  data++;

  // precision: should show the absence of pointer deref
  for (i = 0; i < n; i++) {
    result[i].a = 1;
    data++;
    result[i].b = 1;
    data++;
  }
}


void main() {
  char data[84];
  
  read(data);           // unknown function, reads some data from external input
  parse((int *)data);
}
