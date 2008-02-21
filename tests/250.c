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

void f() {
  int x[50];
  int y;
  if (x[0]  || x[1]  || x[2]  || x[3]  || x[4]  || 
      x[5]  || x[6]  || x[7]  || x[8]  || x[9]  || 
      x[10] || x[11] || x[12] || x[13] || x[14] || 
      x[15] || x[16] || x[17] || x[18] || x[19] || 
      x[20] || x[21] || x[22] || x[23] || x[24] || 
      x[25] || x[26] || x[27] || x[28] || x[29]) {
    y = 1;
   }
}
