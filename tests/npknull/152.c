void main()
{
  int *t[2];
  int x;
  t[1] = &x;
  *t[0] = 1;     // should signal this null pointer deref
}
