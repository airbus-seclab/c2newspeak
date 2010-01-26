int a[2];

int f(void)
{
  return 1;
}

void main(void)
{
  int i = f();
  a[i] = 1;
}
