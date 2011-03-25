
void main()
{
  int *ptr;
  ptr = 0;
  *ptr = 1;     // should signal null pointer deref
}
