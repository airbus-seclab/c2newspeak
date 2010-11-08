Newspeak output
---------------
078.c
void main(void) {
  (078.c:27#6)^int32 i;
  (078.c:28#6)^int32 x;
  (078.c:28#6)^x =(int32) 0;
  (078.c:29#2)^i =(int32) 0;
  (078.c:30#2)^do {
    (078.c:30#2)^while (1) {
      (078.c:30#2)^choose {
       -->
        (078.c:30#2)^guard((10 > i_int32));
       -->
        (078.c:30#2)^guard(! (10 > i_int32));
        (078.c:30#2)^goto lbl1;
      }
      (078.c:31#4)^x =(int32) coerce[-2147483648,2147483647] (1 - x_int32);
      (078.c:30#2)^do {
        (078.c:32#4)^choose {
         -->
          (078.c:32#4)^guard(! (x_int32 ==_int32 0));
          (078.c:33#6)^goto lbl3;
         -->
          (078.c:32#4)^guard((x_int32 ==_int32 0));
        }
        (078.c:35#4)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
      } with lbl3:
    }
  } with lbl1:
}


