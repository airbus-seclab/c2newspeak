// TODO:
// exemples de base
// memcpy de pointeurs, union entre pointeur et entier
// tableaux de pointeurs ??
// arithmetique de pointeur + boucle
// pointeur sur zone mémoire désallouée

/*
#if 0
void main() {
  int *t[10];
  int x;
  
  t[0] = &x;
  *t[0] = 1;
}
#endif


#if 0
void main(){
  int i, *ptr = 0;
  int t[2];
  
  ptr = &t[0];
  ptr = ptr + 100;
  *ptr = 1;
  for (i=0;i<10;i++) {
    *ptr++ = 1;
  }
}
#endif

#if 0
int *f() {
  int x;
  return &x;
}

void main() {
  int *ptr;
  ptr = f();
  *ptr = 1;
}
#endif

#if 0
void main() {
  int *ptr;
  int **q;
  int *t;
  int x;
  int y;
  q = &ptr;
  t = &ptr;
  *q = &x;
  *t = &y;
  *ptr = 1;
}
#endif

#if 0
// there is a bug
struct s {
  int *a;
  int *b;
};

void main() {
  int **ptr;
  int u, v;
  struct s x;
  x.a = &u;
  x.b = &v;

  ptr = &x;

  //  ptr++;

  *ptr = 0;
  
  *x.a = 0;
  *x.b = 0;
}
#endif

#if 0
void f() {
}

void main() {
  void (*fptr)(void);
  fptr = &f;
  (*fptr)();
}
#endif

#if 0
void main() {
  int x;
  __debug_on();
  x = 1;
}
#endif
*/

void main() {
}
