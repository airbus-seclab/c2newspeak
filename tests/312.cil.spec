Newspeak output
---------------
312.c
main() {
  (312.c:27#1076)^ptr ptr;
  (312.c:28#1088)^int32[10] t;
  (312.c:29#1101)^int32 x;
  (312.c:30#1106)^choose {
    | ! (0-_int32 ==_int32 0) -->
      (312.c:30#1106)^2- =(ptr) &_320(1-);
    | (0-_int32 ==_int32 0) -->
      (312.c:30#1106)^2- =(ptr) nil;
  }
}


