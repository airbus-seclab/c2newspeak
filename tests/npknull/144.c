int x;
char *ptr;

void main()
{
  ptr = &x;
  x = 1;
  *ptr = 0; // should not signal any null pointer deref
}
