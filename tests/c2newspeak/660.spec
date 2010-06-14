void main() {
  int32 x;
  x = (!(x == 0)) ? (x + 1) : (x - 1);
}
Newspeak output
---------------
660.c
void main(void) {
  (660.c:27#6)^int32 x;
  (660.c:28#2)^choose {
   -->
    (660.c:28#2)^guard(! (x_int32 ==_int32 0));
    (660.c:28#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
   -->
    (660.c:28#2)^guard((x_int32 ==_int32 0));
    (660.c:28#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 - 1);
  }
}


