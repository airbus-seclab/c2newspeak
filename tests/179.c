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

struct {
  unsigned int a:3; // 0
  unsigned int b:1; // 3
  unsigned int c:8; // 4
  unsigned int d:32; // 32
  unsigned int e:32; // 64
  unsigned int f; // 96
} s0;

struct {
  char a; // 0
  int b; // 32
} s1;

struct {
  char a; // 0
  short b; // 16
  int c; // 32
} s2;

struct {
  int a; // 0
  int b; // 32
  int c; // 64
} s3;

struct {
  int a:3; // 0
  int b:3; // 3
  int c:3; // 6
} s4;

struct {
  int a:10; // 0
  int b:10; // 10
  int c:10; // 20
} s5;

struct {
  int a:30; // 0
  int b:30; // 32
  int c:30; // 64
} s6;

struct {
  int a:30; // 0
  short b:16; // 32
  int c:30; // 64
} s7;

struct {
  char a:8; // 0
  short b:16; // 16
  int c:30; // 32
} s8;

struct {
  int a:8; // 0
  int b:16; // 8
  int c:30; // 32
} s9;

struct {
  int a:8; // 0
  short b:16; // 16
  int c:30; // 32
} s10;

struct {
  short a:8; // 0
  short b:16; // 16
  int c:30; // 32
} s11;

struct {
  int a:8; // 0
  char b:8; // 8
  int c:30; // 32
} s12;

struct {
  int a:8; // 0
  short b:8; // 8
  int c:30; // 32
} s13;

struct {
  short a:8; // 0
  int b:16; // 8
  int c:30; // 32
} s14;

struct {
  int a:8; // 0
  short b:9; // 16
  int c:30; // 32
} s15;

struct {
  int a:8; // 0
  short b:8; // 8
  short c:1; // 16
} s16;

struct {
  int a:7; // 0
  short b:8; // 7
  int c:30; // 32
} s17;

struct {
  int a:9; // 0
  short b:8; // 16
  int c:30; // 32
} s18;

struct {
  char a:1; // 0
  short b:8; // 1
  int c:30; // 32
} s19;

struct {
  char a:1; // 0
  short b:15; // 1
  int c:30; // 32
} s20;

struct {
  char a:1; // 0
  short b:16; // 16
  int c:30; // 32
} s21;

struct {
  char a:2; // 0
  short b:15; // 16
  int c:30; // 32
} s22;

struct {
  int a:8; // 0
  unsigned int b:8; // 8
  int c:30; // 32
} s23;

struct {
  short a;
  short b;
  short c;
} s24; // 48

struct {
  char a;
  char b;
} s25; // 16

struct { 
  int x; 
  struct { char z; } y;
} s26; // 64

struct { 
  int x[10]; 
  struct { char z[10]; } y[10];
} s27; // 1120

struct { 
  char x;
  char y;
} s28; // 16

struct { 
  short x;
  char y;
} s29; // 32

struct { 
  char x;
  char y;
  char z;
} s30; // 24

struct { 
  char x;
  char y;
  short z;
  char t;
} s31; // 48

struct { 
  char x;
  char y;
  char x2;
  char y2;
  int z;
  char t;
} s32; // 96

struct { 
  char x;
  char y;
  char x2;
  char y2;
  char z[4];
  char t;
} s33; // 72

struct { 
  char x;
  char y;
  char x2;
  char y2;
  short z;
  short z2;
  char t;
} s34; // 80

struct { 
  char x;
  char y;
  char x2;
  char y2;
  struct { char a; char b; char c; char d; } z;
  char t;
} s35; // 72

struct { 
  char x;
  char y;
  char x2;
  char y2;
  struct { short a; char c; char d; } z;
  char t;
} s36; // 80

struct { 
  char x;
  char y;
  char x2;
  char y2;
  struct { int a; } z;
  char t;
} s37; // 96

struct { 
  int x;
  struct { char a; } z;
  char t;
} s38; // 64

struct { int x; char a; char b; } s39; // 64

struct { 
  int x;
  struct { int a; char b; } z;
  char t;
} s40; // 128

struct { 
  int x;
  int a; char b;
  char t;
} s41; // 96

struct { 
  int a;
  char b;
  char c;
  char d;
  char e;
  char f;
} s42; // 96

void main() {
}
