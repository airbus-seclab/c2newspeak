struct s1 {
  struct s2 *b;
};

struct s1 x;

struct s2 {
  int m;
};

void f(void) { 
  x.b[0].m;
}

