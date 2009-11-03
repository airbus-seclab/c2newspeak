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

typedef void (*fptr)();

typedef struct {
  char *name;
  fptr func;
} S;

void f() {
  char t[10];
  t[100] = 1;
}

void g() {
  char u[10];
  // precision: should not signal this warning (unreachable code)
  u[100] = 2;
}

S table[2] = {
  {"f", &f},
  {"g", &g}
};

int my_strcmp(char *s1, char *s2) {
  while (*s1 == *s2) {
    if (*s1 == '\0') return 0;
    s1++;
    s2++;
  }
  return (*(unsigned char *)s1) - (*(unsigned char *)s2);
}

fptr find(char *s) {
  int i;
  for (i = 0; i < 2; i++) {
    if (my_strcmp(table[i].name, s) == 0) {
      return table[i].func;
    }
  }
  return 0;
}

int main() {
  char name[10];
  fptr f;
  f = find("f");
  if (f) {
    (*f)();
  }
  return 0;
}
