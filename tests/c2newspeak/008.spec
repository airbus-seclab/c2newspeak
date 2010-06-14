Newspeak output
---------------
008.c
void main(void) {
  (008.c:32#6)^int32 x;
  (008.c:33#2)^choose {
   -->
    (008.c:33#2)^guard((10 > x_int32));
    (008.c:34#4)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
   -->
    (008.c:33#2)^guard(! (10 > x_int32));
  }
}


