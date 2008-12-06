Newspeak output
---------------
312.c
void main(void) {
  (312.c:27#7)^ptr ptr;
  (312.c:28#6)^int32[10] t;
  (312.c:29#6)^int32 x;
  (312.c:30#2)^choose {
    | ! (0-_int32 ==_int32 0) -->
      (312.c:30#2)^2- =(ptr) &_320(1-);
    | (0-_int32 ==_int32 0) -->
      (312.c:30#2)^2- =(ptr) nil;
  }
}


