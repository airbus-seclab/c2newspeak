Newspeak output
---------------
void (020.c:30#5)^main(void) {
  (020.c:31#6)^int32 x;
  (020.c:34#2)^do {
    (020.c:34#2)^do {
      (020.c:32#2)^choose {
       -->
        (020.c:32#2)^guard((x_int32 ==_int32 1));
        (020.c:33#2)^goto lbl2;
       -->
        (020.c:32#2)^choose {
         -->
          (020.c:32#2)^guard((x_int32 ==_int32 2));
          (020.c:34#2)^goto lbl2;
         -->
          (020.c:32#2)^guard(! (x_int32 ==_int32 2));
          (020.c:32#2)^guard(! (x_int32 ==_int32 1));
          (020.c:32#2)^goto lbl1;
        }
      }
    } with lbl2:
    (020.c:35#4)^x =(int32) 1;
  } with lbl1:
}


