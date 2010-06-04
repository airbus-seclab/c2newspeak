struct t1 { int n; };
struct t2 {
  struct t2*next;
  int k;
  int j;
  void *b;
  void *e;
  struct t1 i;
};

struct t2 x = {
  .b=0,
  .e=(void *)~0,
  .i={1},
};


