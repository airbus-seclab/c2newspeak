Newspeak output
---------------
void (077.c:26#5)^main(void) {
  (077.c:27#6)^int32 i;
  (077.c:28#6)^int32 x;
  (077.c:28#6)^x =(int32) 0;
  (077.c:29#7)^i =(int32) 0;
  (077.c:29#2)^do {
    (077.c:29#2)^while (1) {
      (077.c:29#2)^choose {
       -->
        (077.c:29#2)^guard((10 > i_int32));
       -->
        (077.c:29#2)^guard(! (10 > i_int32));
        (077.c:29#2)^goto lbl1;
      }
      (077.c:30#4)^x =(int32) coerce[-2147483648,2147483647] (1 - x_int32);
      (077.c:29#2)^do {
        (077.c:31#4)^choose {
         -->
          (077.c:31#4)^guard(! (x_int32 ==_int32 0));
          (077.c:32#6)^goto lbl3;
         -->
          (077.c:31#4)^guard((x_int32 ==_int32 0));
        }
      } with lbl3:
      (077.c:29#22)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
    }
  } with lbl1:
}


