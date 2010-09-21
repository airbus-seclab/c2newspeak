int x;
int t[2];

void f()
{
  x++;
}


void main()
{
  x = 0;
  f();
  f();
  t[x] = 1;
  f();
  t[x] = 1;
}
