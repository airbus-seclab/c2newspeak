Newspeak output
---------------
void main(void) {
  (225.c:27#6)^int32 x;
  (225.c:29#2)^choose {
   -->
    (225.c:29#2)^guard(! (x_int32 ==_int32 0));
    (225.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    (225.c:29#2)^x =(int32) 1;
   -->
    (225.c:29#2)^guard((x_int32 ==_int32 0));
    (225.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    (225.c:29#2)^x =(int32) ! (x_int32 ==_int32 0);
  }
}


