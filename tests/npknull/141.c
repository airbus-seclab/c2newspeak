int *ptr1;
int *ptr2;
int x;

void main()
{
  ptr1 = &x;
  ptr2 = ptr1;
  *ptr2 = 1;
}
