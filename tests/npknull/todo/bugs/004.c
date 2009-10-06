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

// should not loop forever

extern void *malloc (unsigned int);

typedef struct {
  int *a;
  int *b;
} t;

void f(int *b) {
}

int* g() {
  int *b;
  
  b = malloc(sizeof(int));
  
  return b;
}


void main () {
  int **srv;
  int *s;
  t context;
  int **dc;
  
  context.a = &s;
  
  s = g();
  context.b = g();
  
  *context.b = *context.b;
  
  dc = malloc(sizeof(int**));
  
  *dc = g();
  
  f(*dc);
}
