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

#include <stddef.h>

void bcopy(const void *s1, void *s2, size_t n);
void* memccpy(void *, const void *, int, size_t);
int memcmp(const void *p1, const void *p2, size_t n);
void *memcpy(void *, const void *, size_t n);
void *memmove(void *, const void *, size_t n); 
void *memset(void *p, int c, size_t n);
char *strcat(char *dst, const char *src);
int strncmp(const char *s1, const char *s2, size_t n);
int strcmp(const char *s1, const char *s2);
char *strcpy(char *, const char *);
char *strdup(const char *);
size_t strlen(const char *);
char *strncat(char *, const char *, size_t);
char *strncpy(char*, const char*, size_t);
char *strstr(const char *, const char *);
char *strchr(const char *, int);
