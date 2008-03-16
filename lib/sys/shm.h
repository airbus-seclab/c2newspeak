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
#include <sys/types.h>

struct shmid_ds
{
  //  struct ipc_perm    shm_perm;	/* Operation permission structure. */
  size_t             shm_segsz;	/* Size of segment in bytes. */
  //  pid_t              shm_lpid;	/* Process ID of last operation. */
  //  pid_t              shm_cpid;	/* Process ID of creator. */
  //  shmatt_t           shm_nattch;/* Number of current attaches. */
  //  timestruc_t        shm_atim;	/* Time of last shmat (). */
  //  timestruc_t        shm_dtim;	/* Time of last shmdt (). */
  //  timestruc_t        shm_ctim;	/* Time of last change by shmctl (). */
  //  long               shm_spare4[2];
};

int shmget(key_t key, size_t size, int shmflg);
int shmctl(int shmid, int cmd, struct shmid_ds *buf);
void *shmat(int shmid, const void *shmaddr, int shmflg);
int shmdt(const void *shmaddr);

