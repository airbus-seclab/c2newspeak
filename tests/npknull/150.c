struct s {
  int a;
  int b;
};

struct s x;
struct s* ptr;

void main()
{
  ptr = &x;
  ptr->b = 0;
  ptr->a = 0;
}
