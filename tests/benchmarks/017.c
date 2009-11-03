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

int x;

void onecpy(char* dst, char* src) {
  *dst = *src;
}

int main() {
  int *p1;
  int *p2;
  
  p2 = &x;
  
  onecpy((char*)&p1, (char*)&p2);
  
  // should be a warning here
  // what is the value of p2 here? should be corrupted
  // in Venet's analyses will say p2 points to x and not signal any warning
  // Venet's analyses only check pointer out of bounds (not invalid pointers)
  // TODO: could be made more simple (without a function call)
  *p1 = 1;
  
  return 0;
}
