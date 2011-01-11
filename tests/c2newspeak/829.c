void f(void) {
  int n = 4;
  asm ("" : "=c" (n) : [my_name] "i" (7));
}
