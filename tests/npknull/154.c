int *ptr;
int t;
int **x;

void main()
{
  x = &ptr;
  // at this point should know that x points to (ptr, 0)
  *x = &t;
  // at this point should know that ptr is not null
  *ptr = 1; // should not signal any null pointer deref
}
