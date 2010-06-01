void main(void) {
  int t[4];
  asm ("bla" : "bla" (t[0]*8+3));
}
