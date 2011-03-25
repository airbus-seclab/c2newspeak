struct {
  int *ptr;
  int a;
} x;

int y;

void main()
{
  x.ptr = &y;
  x.a = 0;

  *x.ptr = 1;       // precision: should not raise any alarm
}
