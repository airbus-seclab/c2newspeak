int *ptr;
int t;
int **x;

void main()
{
  x = &ptr;
  *x = &t; // should not signal any null pointer deref
  *ptr = 1;
}
