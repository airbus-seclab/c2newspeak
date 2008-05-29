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

#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H

typedef long int ssize_t;
typedef long int key_t;

typedef unsigned int pthread_t;
typedef unsigned int pthread_mutex_t;
typedef unsigned long int useconds_t;
typedef long int suseconds_t;
typedef long int time_t;
typedef struct {
  int is_initialized;
  void *stackaddr;
  int stacksize;
} pthread_attr_t;

typedef int mode_t;

typedef	int pid_t;
typedef	unsigned short	uid_t;
typedef	unsigned short	gid_t;

typedef unsigned char u_char;

#endif	/* _SYS_TYPES_H */
