Warning: 314.c:29#2: assignment within expression accepted
Newspeak output
---------------
void (314.c:26#5)^main(void) {
  (314.c:27#6)^int32 x;
  (314.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (314.c:29#2)^choose {
   -->
    (314.c:29#2)^guard(! (x_int32 ==_int32 0));
   -->
    (314.c:29#2)^guard((x_int32 ==_int32 0));
  }
}


