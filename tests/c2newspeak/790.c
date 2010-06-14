void f(int i);
void main(void) {
  while (1) {
    int i;
    f(i);
    if (1) {
    lbl:;
    }
    goto lbl;
  }
}
