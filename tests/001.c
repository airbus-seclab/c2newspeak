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

// Some fun with types and declaration
// Note that, since these variables are not used, C2Newspeak is run with
// option --keep-unused-vars, in order to keep them.

void main() {
  // Integer types are normalized according to their size and sign. 
  // Their size, which is architecture dependent, is made explicit.
  int i1;
  unsigned int i2;
  char i3;
  unsigned char i4;

  // Casts (and unions) in C allow programmers to manipulate sequences 
  // of bytes with any type. Consequently, Newspeak distinguishes only 
  // two types of pointers: data and function pointers.
  int *p1;
  unsigned int *p2;
  int (*p3)[10];
  struct { int x; } *p4;
  int (*fp)(int);

  // Newspeak composite data structures are arrays and regions. 
  // A region is a sequence of bytes. Some offsets in the region are 
  // indicated to store values of a given type. Regions can encode both 
  // C structures and unions, while making explicit their architecture 
  // dependent parameters: namely, fields' offsets, paddings and the 
  // overall type size.
  int t[10];
  struct { 
    int x; 
    char y; 
    char* z; 
  } s;
  union { 
    int x;
    char y;
    char* z; 
  } u;
  int t1[10][20];
  int t2[10][20][30];
  struct { 
    int x; 
    struct { char z; } y;
  } s1;
  struct { 
    int x[10]; 
    struct { char z[10]; } y[10];
  } s2;
  struct { 
    int z; 
    union { 
      int x; 
      char y; 
    } t; 
  } s3; 
}
