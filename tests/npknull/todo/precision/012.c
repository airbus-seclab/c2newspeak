char * t[] = { "a", "b", "c" };


void main()
{
  char *ptr;
  int i;
  ptr = t[i];
  *ptr = 1; // should not signal any null pointer
}
