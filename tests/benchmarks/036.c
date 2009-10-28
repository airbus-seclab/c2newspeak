int main() {
  int x;
  char t[10];
  x = -1;
  if (x > sizeof(int)) {
    // should signal this error because -1 is cast to a large unsigned int
    t[100] = 1;
  }
  return 0;
}
