Newspeak output
---------------
008.c
void main(void) {
  (008.c:32#6)^int32 x;
  (008.c:33#2)^choose {
   -->
    (008.c:33#2)^guard((10 > 0-_int32));
    (008.c:34#4)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
   -->
    (008.c:33#2)^guard(! (10 > 0-_int32));
  }
}


