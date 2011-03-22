extern void __display(void);

int x;
int y;
int *ptr1;
int *ptr2;

void main()
{
  ptr1 = &x;
  ptr2 = &y;
  *ptr1 = 1;
  *ptr2 = 2;   // should not generate any alarm
}
