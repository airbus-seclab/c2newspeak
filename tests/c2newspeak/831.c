struct s1 {
  int t[10];
};


struct s2 {
  struct s1 s1;
};

void g(void)
{
  int n;
  n = __builtin_offsetof(struct s2, s1.t[2]);
}
