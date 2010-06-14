struct s {
  int base;
  int x;
  int y;
};

struct s s;

int main(void)
{
  struct s {
    int base;
    int y;
    int x;
  } t;
  int *p;
  p = & t.x; // should be : +64
  p = & s.x; // should be : +32
  return 0;
}
