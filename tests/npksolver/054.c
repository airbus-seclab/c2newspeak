int *ptr;
int a[100];

int main (void)
{
  ptr = 0;
  ptr = &a[100]; // OK
}
