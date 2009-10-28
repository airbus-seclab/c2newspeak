
int x;

int main() {
  int *p;
  
  p = &x;
  
  p = p + 1;
  
  // should be a warning here
  *p = 1;
  
  return 0;
}

