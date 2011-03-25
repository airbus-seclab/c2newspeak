struct s {
  int a;
  int *b;
} t[10];

int x;

void init()
{
  t[0].b = &x;
  t[1].b = &x;
  t[2].b = &x;
  t[3].b = &x;
  t[4].b = &x;
  t[5].b = &x;
  t[6].b = &x;
  t[7].b = &x;
  t[8].b = &x;
  t[9].b = &x;
}

void access()
{
  int i;
  
  *t[i].b = 0;  // should not signal any null pointer deref here.
}

void main()
{
  init();
  access();
}
