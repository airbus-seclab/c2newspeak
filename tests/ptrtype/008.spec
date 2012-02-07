Int (008.c:3#4)^f(void) {
  (008.c:5#4)^!return =(int32) (1 : Int);
}

void (008.c:8#5)^g(void) {
  (008.c:10#8)^Int x;
  (008.c:10#8)^x: Int <- f();
}

