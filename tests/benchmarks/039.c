int main() {
  int *ptr;
  int *y;
  ptr = (int*) 0;
  y = &(*ptr);    // should flag a pointer out of bounds here!!!
  return 0;
}
