struct s {
  int a;
  int b;
};

int main(void)
{
  struct s x;
  struct s *p;
  int *q;
  char *r;
  p = &x;
  q = &p->a; // q = &x.a;
  r = (char *) p;
  r = (char *) (&p->a);
  return 0;
}
