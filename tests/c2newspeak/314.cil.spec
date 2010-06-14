Newspeak output
---------------
314.c
void main(void) {
  (314.c:27#1076)^int32 x;
  (314.c:29#1082)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (314.c:29#1082)^choose {
   -->
    (314.c:29#1082)^guard(! (x_int32 ==_int32 0));
   -->
    (314.c:29#1082)^guard((x_int32 ==_int32 0));
  }
}


