struct s { int a; int *b; } x;

void main()
{
  struct s *t;
  t->b = 1;
}
