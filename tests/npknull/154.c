int *ptr;
int t;
int **x;

void main()
{
  x = &ptr;
  *x = &t; 
  *ptr = 1; // should not signal any null pointer deref
}
