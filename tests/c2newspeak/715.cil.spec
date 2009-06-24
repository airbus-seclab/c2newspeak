Newspeak output
---------------
715.c
void main(void) {
  (715.c:28#1064)^uint32 y;
  (715.c:27#1034)^do {
    (715.c:30#1070)^choose {
     -->
      (715.c:30#1070)^guard(! (coerce[-2147483648,2147483647] 0-_uint32 ==_int32 1));
      (715.c:30#1070)^goto lbl0;
     -->
      (715.c:30#1070)^choose {
       -->
        (715.c:30#1070)^guard((coerce[-2147483648,2147483647] 0-_uint32 ==_int32 1));
        (715.c:30#1070)^goto lbl0;
       -->
      }
    }
  } with lbl0: {
  }
}


