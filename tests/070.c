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

const char t1 = 1;
char const t2 = 1;

const char* t3 = "Hello";
char const * t4 = "Hello";
char* const t5 = "Hello";

const char* t6[1] = { "Hello" };
char const * t7[1] = { "Hello" };
char* const t8[1] = { "Hello" };

typedef char* STRING;

const STRING t9 = "Hello";
STRING const t10 = "Hello";

const STRING t11[1] = { "Hello" };
STRING const t12[1] = { "Hello" };

typedef char* ARRAY[1];

const ARRAY t13 = { "Hello" };
ARRAY const t14 = { "Hello" };

void main() {
  /*
  t1 = 2; // err const var
  t2 = 2; // err const var
  t3 = "Hi";
  t4 = "Hi";
  t5 = "Hi"; // err const var
  t6[0] = "Hi";
  t7[0] = "Hi";
  t8[0] = "Hi"; // err const location
  t9 = "Hi"; // err const var
  t10 = "Hi"; // err const var
  t11[0] = "Hi"; // err const location
  t12[0] = "Hi"; // err const location
  t13[0] = "Hi"; // err const location
  t14[0] = "Hi"; // err const location
*/
}
