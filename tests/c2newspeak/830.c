struct s1 {
 void (*f)(void);
};


struct s2 {
 struct s1 s1;
};

void g(void)
{
  int n;
  n = __builtin_offsetof(struct s2, s1.f);
}
