struct s1 {
  struct s2 *b;
};


struct s2 {
  int m;
};

void f(void) {
  struct s1 x;
  x.b[0].m;
}

