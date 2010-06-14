Newspeak output
---------------
312.c
void main(void) {
  (312.c:27#7)^ptr ptr;
  (312.c:28#6)^int32[10] t;
  (312.c:29#6)^int32 x;
  (312.c:30#2)^choose {
   -->
    (312.c:30#2)^guard(! (x_int32 ==_int32 0));
    (312.c:30#2)^ptr =(ptr) focus320 &(t);
   -->
    (312.c:30#2)^guard((x_int32 ==_int32 0));
    (312.c:30#2)^ptr =(ptr) nil;
  }
}


