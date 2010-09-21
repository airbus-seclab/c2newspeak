int x;
int y;
int *p;

void main (void)
{
  p = &x;
  x = 1;
  y = *p;
}
