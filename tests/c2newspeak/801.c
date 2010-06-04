struct t1 { int n; };
struct t2 {
  struct t2*next;
  void *b;
  void *e;
  struct t1 i;
};

struct t2 x = {
  .b=0,
  .e=0,
  .i={0},
};


