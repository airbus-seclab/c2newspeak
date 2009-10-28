int main() {
  int *p;
   
  // should be a warning here
  *p = 1;
  
  return 0;
}
