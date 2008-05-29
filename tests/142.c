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

void main() {
  float x;
  double y;
  int z;
  
  x = x * x;
  x = x * y;
  x = x * z;
  x = y * x;
  x = y * y;
  x = y * z;
  x = z * x;
  x = z * y;
  x = z * z;

  y = x * x;
  y = x * y;
  y = x * z;
  y = y * x;
  y = y * y;
  y = y * z;
  y = z * x;
  y = z * y;
  y = z * z;

  z = x * x;
  z = x * y;
  z = x * z;
  z = y * x;
  z = y * y;
  z = y * z;
  z = z * x;
  z = z * y;
  z = z * z;
}
