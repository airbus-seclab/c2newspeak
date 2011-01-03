struct s;
typedef int (*fp_t)(struct s*);

void f(fp_t x);

struct s {
  fp_t v;
};

void f(fp_t v){}
