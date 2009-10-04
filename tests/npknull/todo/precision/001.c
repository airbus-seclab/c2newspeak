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

/*
  This code is correct because:
  * argv[argc] is a null pointer.
  * argv[0] through to argv[argc-1] are pointers to strings.
  (from 
  http://publications.gbdirect.co.uk/c_book/chapter10/arguments_to_main.html)
 */

// seems to be correctly analyzed with space invader, need to check this!!
int main(int argc, char** argv) {
  int i;

  for (i = 1; argv[i]; i++) // precision: should not show any null pointer deref
    {
      argv[i][0] = 1;       // precision: should not show any null pointer deref
    }

  return 0;
}
