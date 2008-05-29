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

#ifndef _STDIO_H_
#define _STDIO_H_

#include <stddef.h>

#define	EOF	(-1)

struct _FILE {
  unsigned char *p;
};
typedef struct _FILE FILE;

FILE *stdin;

int printf(const char*, ...);
int fprintf(FILE *, const char *, ...);
int scanf(const char*, ...);
int getc(FILE *stream);
char *fgets(char *s, int n, FILE *stream);
char *gets(char *s);
int snprintf(char *s, size_t n, const char *format, ...);
int sprintf(char *s, const char *format, ...);
FILE *fopen(const char *filename, const char *mode);
int fclose(FILE *stream);
int fputs(const char *s, FILE *stream);
int fputc(int c, FILE *stream);
size_t fread(void *ptr, size_t size, size_t nitems, FILE *stream);
void perror(const char *s);
FILE *tmpfile(void);


#endif /* _STDIO_H_ */
