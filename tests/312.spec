Warning: conditional expression are ugly: use if else instead in 312.c line 30
Newspeak output
---------------
312.c
main() {
  (312.c:27#1077)^ptr ptr;
  (312.c:28#1088)^int32[10] t;
  (312.c:29#1101)^int32 x;
  (312.c:30#1106)^ptr tmp0;
  (312.c:30#1106)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (312.c:30#1106)^0- =(ptr) &_320(2-);
    | (1-_int32 ==_int32 0) -->
      (312.c:30#1106)^0- =(ptr) nil;
  }
  (312.c:30#1106)^3- =(ptr) 0-_ptr;
}


