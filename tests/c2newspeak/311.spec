Warning: 311.c:30#0: init statement expected
Newspeak output
---------------
311.c
void main(void) {
  (311.c:27#6)^int32 i;
  (311.c:28#2)^0- =(int32) 0;
  (311.c:29#2)^do {
    (311.c:29#2)^while (1) {
      (311.c:29#2)^choose {
       -->
        (311.c:29#2)^guard((10 > 0-_int32));
       -->
        (311.c:29#2)^guard(! (10 > 0-_int32));
        (311.c:29#2)^goto lbl1;
      }
      (311.c:29#18)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
}


