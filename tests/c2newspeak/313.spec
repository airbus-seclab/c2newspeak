Warning: 313.c:30#1120: increment statement expected
Newspeak output
---------------
313.c
void main(void) {
  (313.c:27#6)^int32 i;
  (313.c:28#6)^0- =(int32) 0;
  (313.c:28#2)^do {
    (313.c:28#2)^while (1) {
      (313.c:28#2)^choose {
       -->
        (313.c:28#2)^guard(! (0-_int32 > 10));
       -->
        (313.c:28#2)^guard((0-_int32 > 10));
        (313.c:28#2)^goto lbl1;
      }
      (313.c:29#4)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
}


