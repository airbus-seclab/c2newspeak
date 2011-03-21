extern void __display(void);

void f(int *ptr)
{
  *ptr = 1;
}

void main()
{
  int x;
  f(&x);
}
