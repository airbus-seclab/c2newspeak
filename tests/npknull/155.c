extern void __display(void);

int *x;
int y;

void init(int **ptr)
{
  // should know that ptr points to (heap, 0)
  *ptr = &y;
  // should know that (heap, 0) is not null
}

void main()
{
  init(&x);
  // should know that x is not null
  *x = 1;  // should not raise any null pointer deref
}
