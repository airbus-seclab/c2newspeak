char * t = "a";


void main()
{
  char *ptr;
  ptr = t;
  *ptr = 1; // should not signal any null pointer
}
