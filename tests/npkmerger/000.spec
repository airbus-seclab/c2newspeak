void (000-b.c:1#5)^f(void) {
  (000-b.c:2#6)^int32 i;
  (000-b.c:3#2)^i =(int32) 0;
}

void (000-a.c:3#5)^main(void) {
  (000-a.c:4#2)^f();
}

