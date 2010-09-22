int x;
int y;
int *p;

void main (void)
{
  int rnd;
  if (rnd) {
    p = &x;
  } else {
    p = &y;
  }
  x = *p;
}
