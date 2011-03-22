struct s {
  int a;
  int b;
} x;

int *ptr;

void main()
{
  ptr = &x.b;
  *ptr = 1;         // should not signal any null pointer
}
