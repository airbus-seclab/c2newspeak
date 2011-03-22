struct s {
  int a;
  int b;
} x;

void f(char *ptr, int n)
{
  while (n > 0) {
    *ptr = 0;
    ptr++;
    n--;
  }
}

void g(struct s *ptr)
{
  f(&(ptr->b), sizeof(ptr->b));
}

void main()
{
  g(&x);
}
