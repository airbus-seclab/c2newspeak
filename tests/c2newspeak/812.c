struct s;

int f(struct s *p);

struct s
{
  struct s2 *ptr;
};

struct s2
{
  int n;
};

int f(struct s *p){
}
