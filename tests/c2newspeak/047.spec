Newspeak output
---------------
void (047.c:29#5)^main(void) {
  (047.c:30#6)^int32 x;
  (047.c:32#7)^x =(int32) 0;
  (047.c:32#2)^do {
    (047.c:32#2)^while (1) {
      (047.c:32#2)^choose {
       -->
        (047.c:32#2)^guard((10 > x_int32));
       -->
        (047.c:32#2)^guard(! (10 > x_int32));
        (047.c:32#2)^goto lbl1;
      }
      (047.c:32#22)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    }
  } with lbl1:
}


