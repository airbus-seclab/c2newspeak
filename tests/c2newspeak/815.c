struct s {
  int n1;
  int n2;
};

void f(void)
{
  
  int i;
  char t[((long unsigned int)((char *)(sizeof(struct s)) + (sizeof(long unsigned int) - 1)) &~ 
	   (sizeof(long unsigned int) - 1))];
 
}
