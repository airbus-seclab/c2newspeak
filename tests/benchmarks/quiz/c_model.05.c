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

void my_memcpy(char *dst, char *src, int n) {
  int i;
  for (i = 0; i < n; i++) {
    dst[i] = src[i];
  }
}

typedef struct {
  int a;
  int *ptr;
} S;

int witness;
S s1 = {0, &witness};

int main() {
  S s2;
  char t[10];
  witness = 0;
  my_memcpy(&s2, &s1, sizeof(S));
  *s2.ptr = 10000000;
  // soundness: should signal an array out of bounds
  // because of the copy of structure s1 into structure s2
  t[witness] = 0;
  return 0;
}

