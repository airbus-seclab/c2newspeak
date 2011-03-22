int t;
int *ptr;

void main()
{
  while (1) // analysis should not loop forever
    {
      ptr = &t;
    }
}
