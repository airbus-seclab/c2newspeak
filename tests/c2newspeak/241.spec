Newspeak output
---------------
241.c
void f(void) {
  (241.c:27#6)^int32 x;
  (241.c:29#2)^do {
    (241.c:29#2)^while (1) {
      (241.c:30#4)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
      (241.c:29#2)^choose {
       -->
        (241.c:29#2)^guard((10 > x_int32));
       -->
        (241.c:29#2)^guard(! (10 > x_int32));
        (241.c:29#2)^goto lbl1;
      }
    }
  } with lbl1:
}

void g(void) {
  (241.c:35#6)^int32 x;
  (241.c:37#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (241.c:38#2)^do {
    (241.c:38#2)^while (1) {
      (241.c:38#2)^choose {
       -->
        (241.c:38#2)^guard((10 > x_int32));
       -->
        (241.c:38#2)^guard(! (10 > x_int32));
        (241.c:38#2)^goto lbl1;
      }
      (241.c:39#4)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    }
  } with lbl1:
}


