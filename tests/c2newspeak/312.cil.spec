Newspeak output
---------------
312.c
void main(void) {
  (312.c:27#1076)^ptr ptr;
  (312.c:28#1088)^int32[10] t;
  (312.c:29#1101)^int32 x;
  (312.c:30#1106)^choose {
   -->
    (312.c:30#1106)^guard(! (x_int32 ==_int32 0));
    (312.c:30#1106)^ptr =(ptr) focus320 &(t);
   -->
    (312.c:30#1106)^guard((x_int32 ==_int32 0));
    (312.c:30#1106)^ptr =(ptr) nil;
  }
}


