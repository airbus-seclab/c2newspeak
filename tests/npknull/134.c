extern void __display(void);
void main()
{
  int *ptr;
  int x;
  ptr = &x;
  *ptr = 1;    // should not signal any null pointer
}
