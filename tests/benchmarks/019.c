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

// pointers in structures
// interest: low

typedef struct {
  int a;
  int *ptr;
} S;

int witness;

int main() {
  S s;
  char t[10];
  witness = 0;
  s.ptr = &witness;
  *s.ptr = 10000000;
  // should signal an error since s.ptr points to witness that is 
  // then set to 1000000
  t[witness] = 0; 
  return 0;
}

/* Airac Results
Airac5: public-1.0, analyzing test.c
= Progress =
Parsing. (0.00s)
Transforming..... (0.00s)
Computing fixpoint with widening. (0.00s)
Computing fixpoint with narrowing (0.00s)
Inspecting result. (0.00s)
Refining alarms (0.00s)

= Input Program Size =
Procedures: 3
Blocks:     17
Commands:   40

= Analysis Parameters =
Analyze every procedure: no
Inlining depth: 1
Unrolling bound: 0
Expected iterations: 300

= Analysis Times =
Total: 0.00s
Parsing: 0.00s
Transforming: 0.00s
Computing fixpoint with widening: 0.00s
Computing fixpoint with narrowing: 0.00s
Inspecting result: 0.00s
Refining alarms: 0.00s

= Alarms =
Total number of alarms: 1

In procedure main():
Number of alarms: 1
test.c:28:17: Offset: [10000000,10000000] Size: [10,10]
 28:     t[witness] = 0;
Callers: main

-----------------
Results ok
*/
