Newspeak output
---------------
008.c
void main(void) {
  (008.c:32#1212)^int32 x;
  (008.c:33#1217)^choose {
   -->
    (008.c:33#1217)^guard((10 > x_int32));
    (008.c:34#1235)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
   -->
    (008.c:33#1217)^guard(! (10 > x_int32));
  }
}


