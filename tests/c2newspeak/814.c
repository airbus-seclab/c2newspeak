typedef unsigned int size_t;
struct s {
  int n;
  int n2;
};

void main(void)
{
  char buf[(size_t)((char *)(sizeof(struct s)) + (sizeof(size_t) - 1))];
}
