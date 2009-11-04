struct s {
  int a;
  char t[0];
};

struct s2 {
  int a;
  char t[];
};

struct suffix1 {
  int f;
  int x;
  int y;
};

struct suffix2 {
  char u;
  char b;
  int v;
};

void main() {
  struct s* p;
  int result;


  p = read_from_network();
  switch (p->a) with {
    case 0:
      suffix1* q = &(p->t);
      result = q->y; 
      break;
    case 1:
      suffix2* q = &(p->t);
      result = q->v;
      break;
    default:
      return -1;
  }
  
}
