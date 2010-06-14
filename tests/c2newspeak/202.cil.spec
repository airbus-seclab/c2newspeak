Newspeak output
---------------
202.c
void main(void) {
  (202.c:27#1072)^int32 x;
  (202.c:30#1093)^do {
    (202.c:29#1078)^choose {
     -->
      (202.c:29#1078)^goto lbl1;
     -->
    }
  } with lbl1: {
  }
  (202.c:31#1106)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
}


