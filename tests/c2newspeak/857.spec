Warning: 857.c:4#0: comma in expression accepted
Newspeak output
---------------
void (857.c:1#5)^f(void) {
  (857.c:3#8)^int32 x;
  (857.c:4#8)^x =(int32) 0;
  (857.c:4#4)^do {
    (857.c:4#4)^while (1) {
      (857.c:4#12)^x =(int32) 1;
      (857.c:4#4)^choose {
       -->
        (857.c:4#4)^guard(x_int32);
       -->
        (857.c:4#4)^guard(! x_int32);
        (857.c:4#4)^goto lbl1;
      }
      (857.c:4#18)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    }
  } with lbl1:
}


