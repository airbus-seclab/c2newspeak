struct s3;

typedef struct s2 {
  struct s3** p;
};

struct s1;
extern void f(struct s1 *s);

typedef struct s3 {
  struct s1 *p;
};

typedef struct s1 {
  struct s3 v;
};




void f(struct s1 *s){
}
