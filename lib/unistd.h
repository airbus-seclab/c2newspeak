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

#ifndef _UNISTD_H_
#define _UNISTD_H_

#include <sys/types.h>

int chdir(const char *path);
int chown(const char *path, uid_t owner, gid_t group);
int chroot(const char*);
int close(int);
int execl(const char *path, const char *arg0, ... /*, (char *)0 */);
int execlp(const char *file, const char *arg0, ... /*, (char *)0 */);
int fchown(int fildes, uid_t owner, gid_t group);
pid_t fork(void);
char *getcwd(char *buf, size_t size);
char *getpass(const char*);
int pause(void);
ssize_t read(int fildes, void *buf, size_t nbyte);
unsigned int sleep(unsigned int);



#endif /* _UNISTD_H_ */
