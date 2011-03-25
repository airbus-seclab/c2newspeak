// should not fail on this code

void main()
{
  int a;
  a = *((int*)(a * a * a));
}
