void f()
{
  int *ptr;
  ptr = 0;
  *ptr = 1;   // should signal this null pointer deref only once
}

void main()
{
  int *ptr;
  ptr = 0;
  f();
  f();
}
